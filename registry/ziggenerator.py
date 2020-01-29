#!/usr/bin/python3 -i
#
# Copyright (c) 2013-2020 The Khronos Group Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os
import re
from generator import (GeneratorOptions, OutputGenerator, noneStr,
                       regSortFeatures, write)


typeReplacements = {
    'void': 'c_void',
    'char': 'u8',
    'int': 'c_int',
    'float': 'f32',
    'double': 'f64',
    'size_t': 'usize',
    'uint64_t': 'u64',
    'uint32_t': 'u32',
    'uint16_t': 'u16',
    'uint8_t': 'u8',
    'int64_t': 'i64',
    'int32_t': 'i32',
    'int16_t': 'i16',
    'int8_t': 'i8',
}

def valueTypeToZigType(typeName, fixFlagType=False):
    if typeName.startswith('Vk'):
        zigName = typeName[2:]
        if fixFlagType:
            # Some parameters/fields incorrectly use XXFlagBitsEXT instead of XXFlagsEXT as their type.
            # This works fine in C because the two enums are the same size and implicitly cast,
            # but in Zig the XXFlagBitsEXT struct is a namespace and has no size, so we need to
            # correct this to be XXFlagsEXT
            zigName = zigName.replace('FlagBits', 'Flags')
        return zigName
    if typeName.startswith('PFN_vk'):
        return 'PFN_' + typeName[6:]
    if typeName in typeReplacements:
        return typeReplacements[typeName]
    print('Warning: Cant convert type name: ' + typeName)
    return typeName

def enumNameToZigName(enumName, enumTypeExpanded):
        name = enumName.replace(enumTypeExpanded, '')
        if name.startswith('VK_'): name = name[3:]
        if name[0].isdecimal(): name = 'T_' + name
        return name

class ZigGeneratorOptions(GeneratorOptions):
    """ZigGeneratorOptions - subclass of GeneratorOptions.

    Adds options used by ZigOutputGenerator objects during Zig language header
    generation."""

    def __init__(self,
                 prefixText="",
                 indentFuncProto=True,
                 indentFuncPointer=False,
                 **kwargs
                 ):
        """Constructor.
        Additional parameters beyond parent class:

        - prefixText - list of strings to prefix generated header with
        (usually a copyright statement + calling convention macros).
        - indentFuncProto - True if prototype declarations should put each
        parameter on a separate line
        - indentFuncPointer - True if typedefed function pointers should put each
        parameter on a separate line"""
        GeneratorOptions.__init__(self, **kwargs)

        self.prefixText = prefixText
        """list of strings to prefix generated header with (usually a copyright statement + calling convention macros)."""

        self.indentFuncProto = indentFuncProto
        """True if prototype declarations should put each parameter on a separate line"""

        self.indentFuncPointer = indentFuncPointer
        """True if typedefed function pointers should put each parameter on a separate line"""

class ZigOutputGenerator(OutputGenerator):
    """Generates Zig-language API interfaces."""

    # This is an ordered list of sections in the header file.
    TYPE_SECTIONS = ['include', 'define', 'basetype', 'handle', 'enum',
                     'group', 'bitmask', 'funcpointer', 'struct']
    ALL_SECTIONS = TYPE_SECTIONS + ['commandWrapper', 'command']

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Internal state - accumulators for different inner block text
        self.sections = {section: [] for section in self.ALL_SECTIONS}
        self.feature_not_empty = False
        self.may_alias = None
        self.handleTypes = { 'VkCString': True }

    def beginFile(self, genOpts):
        OutputGenerator.beginFile(self, genOpts)
        # Zig-specific

        # User-supplied prefix text, if any (list of strings)
        if genOpts.prefixText:
            for s in genOpts.prefixText:
                write(s, file=self.outFile)
        write('pub const CString = [*]const u8;', file=self.outFile)

    def beginFeature(self, interface, emit):
        # Start processing in superclass
        OutputGenerator.beginFeature(self, interface, emit)
        # C-specific
        # Accumulate includes, defines, types, enums, function pointer typedefs,
        # end function prototypes separately for this feature. They're only
        # printed in endFeature().
        self.sections = {section: [] for section in self.ALL_SECTIONS}
        self.feature_not_empty = False

    def endFeature(self):
        "Actually write the interface to the output file."
        # C-specific
        if self.emit:
            if self.feature_not_empty:
                if self.genOpts.conventions.writeFeature(None, self.genOpts.filename):
                    self.newline()
                    write('pub const', self.featureName[3:], '= 1;', file=self.outFile)
                    for section in self.TYPE_SECTIONS:
                        contents = self.sections[section]
                        if contents:
                            write('\n'.join(contents), file=self.outFile)
                    if self.sections['command']:
                        write('\n'.join(self.sections['command']), file=self.outFile)
                    #if self.sections['commandWrapper']:
                        #write('\n'.join(self.sections['commandWrapper']), file=self.outFile)
        # Finish processing in superclass
        OutputGenerator.endFeature(self)

    def appendSection(self, section, text):
        "Append a definition to the specified section"
        # self.sections[section].append('SECTION: ' + section + '\n')
        self.sections[section].append(text)
        self.feature_not_empty = True

    def genType(self, typeinfo, name, alias):
        "Generate type."
        OutputGenerator.genType(self, typeinfo, name, alias)
        typeElem = typeinfo.elem

        # Vulkan:
        # Determine the category of the type, and the type section to add
        # its definition to.
        # 'funcpointer' is added to the 'struct' section as a workaround for
        # internal issue #877, since structures and function pointer types
        # can have cross-dependencies.
        category = typeElem.get('category')
        if category == 'funcpointer':
            section = 'struct'
        else:
            section = category

        if alias:
            # If the type is an alias, just emit a typedef declaration
            body = 'pub const ' + valueTypeToZigType(name) + ' = ' + valueTypeToZigType(alias) + ';'
            self.appendSection(section, body)
        elif category in ('struct', 'union'):
            # If the type is a struct type, generate it using the
            # special-purpose generator.
            self.genStruct(typeinfo, name)
        elif category == 'bitmask':
            self.genFlags(typeinfo, name)
        else:
            self.genSimpleType(typeinfo, name, section)

    def typeMayAlias(self, typeName):
        if not self.may_alias:
            # First time we've asked if a type may alias.
            # So, let's populate the set of all names of types that may.

            # Everyone with an explicit mayalias="true"
            self.may_alias = set(typeName
                                 for typeName, data in self.registry.typedict.items()
                                 if data.elem.get('mayalias') == 'true')

            # Every type mentioned in some other type's parentstruct attribute.
            parent_structs = (otherType.elem.get('parentstruct')
                              for otherType in self.registry.typedict.values())
            self.may_alias.update(set(x for x in parent_structs
                                      if x is not None))
        return typeName in self.may_alias

    def genSimpleType(self, typeinfo, typeName, section):
        typeElem = typeinfo.elem
        category = typeElem.get('category')
        body = ''
        if category == 'basetype':
            rawType = typeElem.find('type').text
            body = 'pub const ' + valueTypeToZigType(typeName) + ' = ' + valueTypeToZigType(rawType) + ';'
        elif category == 'define':
            commentText = ''
            text = typeElem.text
            while text.startswith('//'):
                try:
                    newline = text.index('\n') + 1
                except:
                    newline = 2 # just include the //
                commentText += text[0:newline]
                text = text[newline:]
                            
            innerTypeElem = typeElem.find('type')
            
            definition = ''
            
            # special cases
            if typeName == 'VK_MAKE_VERSION':
                definition = "pub fn MAKE_VERSION(major: u32, minor: u32, patch: u32) u32 { return @shlExact(major, 22) | @shlExact(minor, 12) | patch; }"
            elif typeName == 'VK_VERSION_MAJOR':
                definition = "pub fn VERSION_MAJOR(version: u32) u32 { return version >> 22; }"
            elif typeName == 'VK_VERSION_MINOR':
                definition = "pub fn VERSION_MINOR(version: u32) u32 { return (version >> 12) & 0x3ff; }"
            elif typeName == 'VK_VERSION_PATCH':
                definition = "pub fn VERSION_PATCH(version: u32) u32 { return version & 0xfff; }"
            elif innerTypeElem is not None and innerTypeElem.text == 'VK_MAKE_VERSION':
                definition = 'pub const ' + typeName[3:] + ' = MAKE_VERSION' + innerTypeElem.tail.replace(')', ');')
            elif typeName in ('VK_DEFINE_HANDLE', 'VK_DEFINE_NON_DISPATCHABLE_HANDLE', 'VK_NULL_HANDLE'):
                pass # ignore these definitions
            elif not innerTypeElem:
                if text.startswith('#define'):
                    definition = 'pub const ' + typeName[3:] + ' = ' + typeElem.find('name').tail.strip() + ';'
                else:
                    definition = 'pub const ' + typeName + ' = @OpaqueType();'
            else:
                print("Warning: Couldn't generate zig definition for define " + typeName)

            if definition:
                body = commentText + definition
        elif category == 'handle':
            body = 'pub const ' + valueTypeToZigType(typeName) + ' = *@OpaqueType();'
            self.handleTypes[typeName] = True
        elif category == 'include':
            pass # don't generate includes
        else:
            body = noneStr(typeElem.text)
            for elem in typeElem:
                body += noneStr(elem.text) + noneStr(elem.tail)

        if body:
            if '\n' in body[0:-1]:
                body += '\n'
            self.appendSection(section, body)



    def genStruct(self, typeinfo, typeName):
        """Generate struct (e.g. C "struct" type).

        This is a special case of the <type> tag where the contents are
        interpreted as a set of <member> tags instead of freeform C
        C type declarations. The <member> tags are just like <param>
        tags - they are a declaration of a struct or union member.
        Only simple member declarations are supported (no nested
        structs etc.)

        If alias is not None, then this struct aliases another; just
        generate a typedef of that alias."""
        OutputGenerator.genStruct(self, typeinfo, typeName, None)

        typeElem = typeinfo.elem

        body = 'pub const ' + valueTypeToZigType(typeName) + ' = extern ' + typeElem.get('category') + ' {\n'

        allMembers = list(typeElem.findall('.//member'))
        for member in allMembers:
            body += self.makeZigParamDecl(allMembers, member, True)
            body += ',\n'

        body += '};\n'

        self.appendSection('struct', body)

    def makeZigParamDecl(self, allParams, param, includeDefault=False):
        """Return a string which is an indented, formatted
        declaration for a `<param>` or `<member>` block (e.g. function parameter
        or structure/union member).

        - param - Element (`<param>` or `<member>`) to format"""
        
        tokenize = lambda text : re.sub(r'(\S)\*', r'\1 *', re.sub(r'\*(\S)', r'* \1', text)).split()
        parts = []
        if param.text: parts.extend(tokenize(param.text))
        for elem in param:
            if elem.text: parts.extend(tokenize(elem.text))
            if elem.tail: parts.extend(tokenize(elem.tail))

            if self.should_insert_may_alias_macro and self.genOpts.conventions.is_voidpointer_alias(elem.tag, text, tail):
                # OpenXR-specific macro insertion - but not in apiinc for the spec
                tail = self.genOpts.conventions.make_voidpointer_alias(tail)
        
        parts = [x for x in parts if x != 'struct']

        # parts is of the form
        # (const)? <valueType> ((const)? \*)* <name> (\[ <bufferLen> \])*      
        buffers = []
        while parts[-1].endswith(']'):
            if parts[-1].startswith('['):
                buffers.append(parts.pop())
            else:
                parts.pop()
                length = parts.pop()
                parts.pop()
                buffers.append('[' + length[3:] + ']')
        
        
        if parts[0] == 'const':
            parts[0] = parts[1]
            parts[1] = 'const'
            # check for redundant const
            if len(parts) > 2 and parts[2] == 'const':
                parts.pop(2)
        
        name = parts.pop()
        valueType = parts.pop(0)
        parts.reverse()
        
        numStars = 0
        for part in parts:
            if part == '*':
                numStars += 1
        
        lengths = noneStr(param.get('len')).split(',')
        if len(lengths) == 1 and len(lengths[0]) == 0: lengths = []
        while len(lengths) < numStars: lengths.append('1')
        while len(lengths) > numStars: lengths.pop()
        
        if valueType == 'char' and len(parts) >= 2 and parts[-1] == 'const' and parts[-2] == '*' and lengths[-1] == 'null-terminated':
            lengths.pop()
            parts.pop()
            parts.pop()
            valueType = 'VkCString'

        isParamOptional = param.get('optional') == 'true'
        defaultValue = '0'

        typeDecl = ''.join(buffers)
        starIndex = 0
        parenDepth = 0
        for part in parts:
            if part == '*':
                defaultValue = 'null'
                isVoid = valueType == 'void'
                isArray = lengths[starIndex] != '1'
                isOptional = starIndex == 0 and (isParamOptional or isVoid)
                if isOptional:
                    if len(typeDecl) > 0:
                        typeDecl += '('
                        parenDepth += 1
                    typeDecl += '?'
                if isArray and not isVoid:
                    typeDecl += '[*]'
                else:
                    typeDecl += '*'
                starIndex += 1

            else:
                typeDecl += part

        if starIndex == 0 and isParamOptional and self.isValueTypePointer(valueType):
            defaultValue = 'null'
            # TODO: Only if pointer type!!
            if len(typeDecl) > 0:
                typeDecl += '('
                parenDepth += 1
            typeDecl += '?'

        # if the type ends with const we need a space before the value
        if len(typeDecl) > 0 and typeDecl[-1].isalpha():
            typeDecl += ' '

        typeDecl += valueTypeToZigType(valueType, fixFlagType=True)
        for x in range(parenDepth):
            typeDecl += ')'

        # construct the declaration
        paramdecl = '    ' + name + ': ' + typeDecl

        # add a default initialization value
        if includeDefault:
            if isParamOptional:
                paramdecl += ' = ' + defaultValue
            elif name == 'pNext' and valueType == 'void':
                paramdecl += ' = null'
            elif len(lengths) >= 1:
                # determine if the parent is optional
                parentElem = None
                for m in allParams:
                    mName = m.find('name').text
                    if mName == lengths[0]:
                        parentElem = m
                        break
                if parentElem and parentElem.get('optional') == 'true':
                    paramdecl += ' = undefined'
            else:
                valuesStr = param.get('values')
                if valuesStr:
                    values = valuesStr.split(',')
                    if len(values) == 1 and values[0].startswith('VK_STRUCTURE_TYPE_'):
                        paramdecl += ' = ' + values[0].replace('VK_STRUCTURE_TYPE_', '.')

        return paramdecl

    def genGroup(self, groupinfo, groupName, alias=None):
        """Generate groups (e.g. C "enum" type).

        These are concatenated together with other types.

        If alias is not None, it is the name of another group type
        which aliases this type; just generate that alias."""
        OutputGenerator.genGroup(self, groupinfo, groupName, alias)
        groupElem = groupinfo.elem

        # After either enumerated type or alias paths, add the declaration
        # to the appropriate section for the group being defined.
        if groupElem.get('type') == 'bitmask':
            section = 'bitmask'
        else:
            section = 'group'

        if alias:
            # If the group name is aliased, just emit a typedef declaration
            # for the alias.
            body = 'pub const ' + valueTypeToZigType(groupName) + ' = ' + valueTypeToZigType(alias) + ';\n'
            self.appendSection(section, body)
        else:
            (section, body) = self.buildEnumZigDecl(groupinfo, groupName)
            self.appendSection(section, "\n" + body)

    def genEnum(self, enuminfo, name, alias):
        """Generate enumerants.

        <enum> tags may specify their values in several ways, but are usually
        just integers."""
        OutputGenerator.genEnum(self, enuminfo, name, alias)
        (_, strVal) = self.enumToValue(enuminfo.elem, False)
        body = 'pub const ' + name[3:] + ' = ' + strVal + ';'
        self.appendSection('enum', body)

    def buildEnumZigDecl(self, groupinfo, groupName):
        """Generate the Zig declaration for an enum"""
        groupElem = groupinfo.elem

        if groupElem.get('type') == 'bitmask':
            return self.buildEnumZigDecl_Bitmask(groupinfo, groupName)
        else:
            return self.buildEnumZigDecl_Enum(groupinfo, groupName)

    def genFlags(self, typeinfo, name, alias=None):
        body = ''
        if alias:
            body = 'pub const ' + valueTypeToZigType(name) + ' = ' + valueTypeToZigType(alias) + ';'
        elif not typeinfo.elem.get('requires'):
            body = 'pub const ' + valueTypeToZigType(name) + ' = Flags;'
        
        if body:
            self.appendSection("bitmask", body)
            

    def buildEnumZigDecl_Bitmask(self, groupinfo, groupName):
        """Generate the Zig declaration for an "enum" that is actually a
        set of flag bits"""
        groupElem = groupinfo.elem
        flagBitsName = groupinfo.elem.get('name')
        flagTypeName = flagBitsName.replace('FlagBits', 'Flags')
        rootName = flagBitsName.replace('FlagBits', '')
        expandName = re.sub(r'([0-9a-z_])([A-Z0-9])', r'\1_\2', rootName).upper() + '_'
        zigFlagTypeName = valueTypeToZigType(flagTypeName)

        # Prefix
        body = "pub const " + zigFlagTypeName + " = Flags;\n"
        body += "pub const " + valueTypeToZigType(flagBitsName) + " = struct {\n"

        declBody = ''
        aliasBody = ''

        # Loop over the nested 'enum' tags.
        for elem in groupElem.findall('enum'):
            # Convert the value to an integer and use that to track min/max.
            # Values of form -(number) are accepted but nothing more complex.
            # Should catch exceptions here for more complex constructs. Not yet.
            (numVal, strVal) = self.enumToValue(elem, True)
            name = enumNameToZigName(elem.get('name'), expandName)

            if numVal is not None:
                declBody += "    pub const {}: {} = {};\n".format(name, zigFlagTypeName, strVal)
            else:
                # this is an alias
                strVal = enumNameToZigName(strVal, expandName)
                aliasBody += '    pub const {} = {};\n'.format(name, strVal)

        body += declBody
        if declBody and aliasBody:
            body += '\n'
        body += aliasBody

        # Postfix
        body += '};\n'

        return ("bitmask", body)

    def buildEnumZigDecl_Enum(self, groupinfo, groupName):
        """Generate the Zig declaration for an enumerated type"""
        groupElem = groupinfo.elem

        # Break the group name into prefix and suffix portions for range
        # enum generation
        expandName = re.sub(r'([0-9a-z_])([A-Z0-9])', r'\1_\2', groupName).upper()
        expandPrefix = expandName
        expandSuffix = ''
        expandSuffixMatch = re.search(r'[A-Z][A-Z]+$', groupName)
        if expandSuffixMatch:
            expandSuffix = '_' + expandSuffixMatch.group()
            # Strip off the suffix from the prefix
            expandPrefix = expandName.rsplit(expandSuffix, 1)[0]
        expandPrefix = expandPrefix + '_'

        # Prefix
        body = ["pub const %s = extern enum {" % valueTypeToZigType(groupName)]

        # Get a list of nested 'enum' tags.
        enums = groupElem.findall('enum')

        # Check for and report duplicates, and return a list with them
        # removed.
        enums = self.checkDuplicateEnums(enums)

        # Accumulate non-numeric enumerant values separately and append
        # them following the numeric values, to allow for aliases.
        # NOTE: this doesn't do a topological sort yet, so aliases of
        # aliases can still get in the wrong order.
        aliasText = []

        for elem in enums:
            # Convert the value to an integer and use that to track min/max.
            # Values of form -(number) are accepted but nothing more complex.
            # Should catch exceptions here for more complex constructs. Not yet.
            (numVal, strVal) = self.enumToValue(elem, True)
            name = enumNameToZigName(elem.get('name'), expandPrefix)

            # Extension enumerants are only included if they are required
            if self.isEnumRequired(elem):
                if numVal is not None:
                    decl = "    {} = {},".format(name, strVal)
                    body.append(decl)
                else:
                    strVal = enumNameToZigName(strVal, expandPrefix)
                    decl = "    pub const {} = {};".format(name, strVal)
                    aliasText.append(decl)

        # Now append the non-numeric enumerant values
        body.extend(aliasText)

        # Postfix
        body.append("};")

        return ('group', '\n'.join(body))

    def genCmd(self, cmdinfo, name, alias):
        "Command generation"
        OutputGenerator.genCmd(self, cmdinfo, name, alias)

        # if alias:
        #     prefix = '// ' + name + ' is an alias of command ' + alias + '\n'
        # else:
        #     prefix = ''

        prefix = ''
        decls = self.makeZigDecls(cmdinfo.elem)
        self.appendSection('command', prefix + decls[0] + '\n')
        self.appendSection('commandWrapper', decls[1])

    def makeZigDecls(self, cmd):
        """Return Zig prototype and function pointer typedef for a
        `<command>` Element, as a two-element list of strings.

        - cmd - Element containing a `<command>` tag"""
        proto = cmd.find('proto')
        params = cmd.findall('param')
        # Begin accumulating prototype and typedef strings
        pdecl = 'pub extern fn '
        tdecl = 'typedef '

        # Insert the function return type/name.
        # For prototypes, add APIENTRY macro before the name
        # For typedefs, add (APIENTRY *<name>) around the name and
        #   use the PFN_cmdnameproc naming convention.
        # Done by walking the tree for <proto> element by element.
        # etree has elem.text followed by (elem[i], elem[i].tail)
        #   for each child element and any following text
        # Leading text
        returnType = noneStr(proto.text)
        # For each child element, if it's a <name> wrap in appropriate
        # declaration. Otherwise append its contents and tail contents.
        for elem in proto:
            text = noneStr(elem.text)
            tail = noneStr(elem.tail)
            if elem.tag == 'name':
                pdecl += self.makeProtoName(text, tail)
                tdecl += self.makeTypedefName(text, tail)
            else:
                returnType += text + tail

        returnType = returnType.strip()
        if returnType != 'void':
            returnType = valueTypeToZigType(returnType, fixFlagType=True)
        tdecl += returnType

        # Squeeze out multiple spaces - there is no indentation
        pdecl = ' '.join(pdecl.split())
        tdecl = ' '.join(tdecl.split())

        # Now add the parameter declaration list, which is identical
        # for prototypes and typedefs. Concatenate all the text from
        # a <param> node without the tags. No tree walking required
        # since all tags are ignored.
        # Uses: self.indentFuncProto
        # self.indentFuncPointer
        n = len(params)
        # Indented parameters
        if n > 0:
            indentdecl = '(\n'
            indentdecl += ',\n'.join(self.makeZigParamDecl(params, p)
                                     for p in params)
            indentdecl += ',\n)'
        else:
            indentdecl = '()'
        # Non-indented parameters
        paramdecl = '('
        if n > 0:
            paramnames = (''.join(t for t in p.itertext())
                          for p in params)
            paramdecl += ', '.join(paramnames)
        else:
            paramdecl += ''
        paramdecl += ")"
        return [pdecl + indentdecl + ' ' + returnType + ';', tdecl + paramdecl + ';']
    
    def makeProtoName(self, name, tail):
        """Turn a `<proto>` `<name>` into C-language prototype
        and typedef declarations for that name.

        - name - contents of `<name>` tag
        - tail - whatever text follows that tag in the Element"""
        return name + tail

    def makeTypedefName(self, name, tail):
        """Make the function-pointer typedef name for a command."""
        return '(PFN_' + name + tail + ')'
        
    def enumToValue(self, elem, needsNum):
        """Parse and convert an `<enum>` tag into a value.

        Returns a list:

        - first element - integer representation of the value, or None
          if needsNum is False. The value must be a legal number
          if needsNum is True.
        - second element - string representation of the value

        There are several possible representations of values.

        - A 'value' attribute simply contains the value.
        - A 'bitpos' attribute defines a value by specifying the bit
          position which is set in that value.
        - An 'offset','extbase','extends' triplet specifies a value
          as an offset to a base value defined by the specified
          'extbase' extension name, which is then cast to the
          typename specified by 'extends'. This requires probing
          the registry database, and imbeds knowledge of the
          API extension enum scheme in this function.
        - An 'alias' attribute contains the name of another enum
          which this is an alias of. The other enum must be
          declared first when emitting this enum."""
        name = elem.get('name')
        numVal = None
        if 'value' in elem.keys():
            value = elem.get('value')
            value = value.replace("0ULL", "u64(0)").replace("0U", "u32(0)")
            if value[-1] == 'f':
                value = 'f32(' + value[:-1] + ')'
            # print('About to translate value =', value, 'type =', type(value))
            if needsNum:
                numVal = int(value, 0)
            self.logMsg('diag', 'Enum', name, '-> value [', numVal, ',', value, ']')
            return [numVal, value]
        if 'bitpos' in elem.keys():
            value = elem.get('bitpos')
            bitpos = int(value, 0)
            numVal = 1 << bitpos
            value = '0x%08x' % numVal
            if bitpos >= 32:
                value = value + 'ULL'
            self.logMsg('diag', 'Enum', name, '-> bitpos [', numVal, ',', value, ']')
            return [numVal, value]
        if 'offset' in elem.keys():
            # Obtain values in the mapping from the attributes
            enumNegative = False
            offset = int(elem.get('offset'), 0)
            extnumber = int(elem.get('extnumber'), 0)
            extends = elem.get('extends')
            if 'dir' in elem.keys():
                enumNegative = True
            self.logMsg('diag', 'Enum', name, 'offset =', offset,
                        'extnumber =', extnumber, 'extends =', extends,
                        'enumNegative =', enumNegative)
            # Now determine the actual enumerant value, as defined
            # in the "Layers and Extensions" appendix of the spec.
            numVal = self.extBase + (extnumber - 1) * self.extBlockSize + offset
            if enumNegative:
                numVal *= -1
            value = '%d' % numVal
            # More logic needed!
            self.logMsg('diag', 'Enum', name, '-> offset [', numVal, ',', value, ']')
            return [numVal, value]
        if 'alias' in elem.keys():
            return [None, elem.get('alias')]
        return [None, None]
        
    def isValueTypePointer(self, typeName):
        return typeName.startswith('PFN_') or typeName in self.handleTypes