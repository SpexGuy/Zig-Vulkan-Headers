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

nullTerminatedSizes = [
    'VK_MAX_PHYSICAL_DEVICE_NAME_SIZE',
    'VK_MAX_EXTENSION_NAME_SIZE',
    'VK_MAX_DESCRIPTION_SIZE',
    'VK_MAX_DRIVER_NAME_SIZE',
    'VK_MAX_DRIVER_INFO_SIZE',
]

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

def enumNameToZigName(name, enumTypeExpanded, expandedSuffix):
    if name.startswith(enumTypeExpanded): name = name[len(enumTypeExpanded):]
    elif name.startswith('VK_'): name = name[3:]
    if name.endswith(expandedSuffix): name = name[:-len(expandedSuffix)] 
    if name[0].isdecimal(): name = 'T_' + name
    return name
    
def flagNameToZigName(name, enumTypeExpanded, expandedSuffix):
    name = enumNameToZigName(name, enumTypeExpanded, expandedSuffix)
    try:
        bitPos = name.rindex("_BIT");
        mainName = name[:bitPos]
        vendor = name[bitPos+4:]
        if vendor and vendor[0] == '_':
            vendor = vendor[1:]
    except:
        mainName = name
        vendor = ''
    # inefficient convert ALL_CAPS_EXT to allCapsExt
    # TODO don't convert the vendor tag
    name = mainName.replace('_', ' ').title().replace(' ', '')
    name = name[0].lower() + name[1:] + vendor
    if name == 'type' or name == 'error':
        name += 'Bit';
    return name

def splitTypeName(name):
    """ Split the vendor from the name.  splitTypeName('FooTypeEXT') => ('FooType', 'EXT'). """ 
    suffixMatch = re.search(r'[A-Z][A-Z]+$', name)
    prefix = name
    suffix = ''
    if suffixMatch:
        suffix = suffixMatch.group()
        prefix = name[:-len(suffix)]
    return (prefix, suffix)
    
def bitIndex(flag):
    low = (flag & -flag)
    lowBit = -1
    while (low):
        low >>= 1
        lowBit += 1
    return lowBit

class ZigValueType:
    TYPE_OTHER = 0,
    TYPE_POINTER = 1,
    TYPE_HANDLE = 2,
    TYPE_FLAGS = 3,
    def __init__(self, cValueType, declValueType, metaType, isOptional):
        self.cValueType = cValueType
        self.declValueType = declValueType
        self.metaType = metaType
        self.isOptional = isOptional

class ZigIndirect:
    TYPE_BUFFER = 1
    TYPE_POINTER = 2
    TYPE_SLICE = 3

    def __init__(self, type, cLength, isOptional, isConst, isNullTerminated=False):
        self.type = type
        self.cLength = cLength
        self.isOptional = isOptional
        self.isConst = isConst
        self.isNullTerminated = isNullTerminated
        
        self.lenParam = None
        """ After linking, pointer to the ZigParam with the length of this pointer """

class ZigParam:
    def __init__(self, name, valueType, indirections, defaultValue, isOptional):
        self.name = name
        """ The name of this parameter or member """
        
        self.valueType = valueType
        """ The ZigValueType of this parameter """

        self.indirections = indirections
        """ The []ZigIndirect buffers and pointers, from outermost to innermost """
        
        self.defaultValue = defaultValue
        """ If the member should be initialized to a default value, the default value literal """
        
        self.isOptional = isOptional

        self.buffers = []
        """ After linking, the list of parameters for which this parameter is the length """
        
    def isOutParam(self):
        if self.valueType.cValueType == 'void': return False
        if len(self.indirections) == 0: return False
        outermost = self.indirections[0]
        return outermost.type == ZigIndirect.TYPE_POINTER and not outermost.isConst and outermost.cLength == '1' and not outermost.isOptional
    
    def isValueInParam(self):
        if self.valueType.cValueType == 'void': return False
        if len(self.indirections) == 0: return False
        outermost = self.indirections[0]
        return outermost.type == ZigIndirect.TYPE_POINTER and outermost.isConst and outermost.cLength == '1' and not outermost.isOptional
    
    def isMutableBufferLen(self):
        if not self.buffers: return False
        if len(self.indirections) == 0: return False
        outermost = self.indirections[0]
        return outermost.type == ZigIndirect.TYPE_POINTER and not outermost.isConst and outermost.cLength == '1'
        
    def isConstBufferLen(self):
        if not self.buffers: return False
        return len(self.indirections) == 0
        
    def isBuffer(self):
        if len(self.indirections) == 0: return False
        outermost = self.indirections[0]
        return outermost.type == ZigIndirect.TYPE_POINTER and outermost.cLength != '1'
    
    def isDirectFlags(self):
        return not self.indirections and not self.valueType.isOptional and self.valueType.metaType == ZigValueType.TYPE_FLAGS

    def isDirectHandle(self):
        return not self.indirections and not self.valueType.isOptional and self.valueType.metaType == ZigValueType.TYPE_HANDLE

    def getValueParam(self):
        """ Get a parameter that has one less indirection """
        name = self.bufferName()
        return ZigParam(name, self.valueType, self.indirections[1:], None, self.isOptional)
        
    def bufferName(self):
        name = self.name
        if name[0] == 'p' and (name[1].isupper() or name[1] == 'p'):
            name = name[1:2].lower() + name[2:]
        return name

    def getBufferParam(self):
        ind0 = self.indirections[0]
        name = self.bufferName()
        valueType = self.valueType
        if valueType.cValueType == 'void':
            # translate to []u8
            valueType = ZigValueType(valueType.cValueType, 'u8', valueType.metaType, valueType.isOptional)
        value = ZigParam(name, valueType, self.indirections[1:], None, self.isOptional)
        value.indirections.insert(0, ZigIndirect(ZigIndirect.TYPE_SLICE, ind0.cLength, False, ind0.isConst))
        return value
        
    def getCopy(self):
        return ZigParam(self.name, self.valueType, self.indirections, self.defaultValue, self.isOptional)
        
        
    def link(params):
        for param in params:
            if len(param.indirections) >= 1 and param.indirections[0].type == ZigIndirect.TYPE_POINTER:
                lenName = param.indirections[0].cLength
                for other in params:
                    if other.name == lenName:
                        param.indirections[0].lenParam = other
                        other.buffers.append(param)
                        break
    
    def typeDecl(self, workaround3325, isRawApi):
        parenDepth = 0
        decl = ''
        for indirect in self.indirections:
            if indirect.type == ZigIndirect.TYPE_BUFFER:
                zigLen = indirect.cLength
                if zigLen.startswith('VK_'): zigLen = zigLen[3:]
                decl += '[' + zigLen
                if indirect.isNullTerminated:
                    decl += '-1:0'
                decl += ']'
            elif indirect.type == ZigIndirect.TYPE_POINTER:
                isVoid = self.valueType.cValueType == 'void'
                isArray = indirect.cLength != '1'
                isOptional = indirect.isOptional or (workaround3325 and isArray)
                if isOptional:
                    if len(decl) > 0:
                        decl += '('
                        parenDepth += 1
                    decl += '?'
                if isArray and not isVoid:
                    if indirect.isNullTerminated:
                        decl += '[*:0]'
                    else:
                        decl += '[*]'
                else:
                    decl += '*'
            elif indirect.type == ZigIndirect.TYPE_SLICE:
                assert(not indirect.isOptional)
                if indirect.isNullTerminated:
                    decl += '[:0]'
                else:
                    decl += '[]'
            
            if indirect.isConst:
                decl += 'const'
                
        if self.valueType.isOptional:
            if len(decl) > 0:
                decl += '('
                parenDepth += 1
            decl += '?'

        if decl and self.valueType.metaType == ZigValueType.TYPE_FLAGS:
            # swap the order of align(4) and last const for consistency with zig fmt
            if decl.endswith('const'):
                decl = decl[:-5] + 'align(4) const '
            else:
                decl += 'align(4) '

        if len(decl) > 0 and decl[-1].isalpha():
            decl += ' '

        decl += self.valueType.declValueType

        if self.isDirectFlags() and isRawApi:
            decl += '.IntType'
            
        if self.isDirectHandle() and isRawApi:
            decl += '.IntType'
        
        for _ in range(parenDepth):
            decl += ')'
            
        return decl
    
    def structDecl(self, workaround3325):
        decl = self.name + ': ' + self.typeDecl(workaround3325, False)
        if self.isDirectFlags():
            decl += ' align(4)'
        if self.defaultValue:
            decl += ' = ' + self.defaultValue
        return decl
    
    def paramDecl(self, workaround3325, isRawApi):
        return self.name + ': ' + self.typeDecl(workaround3325, isRawApi)
        
    def varDecl(self, workaround3325):
        decl = 'var ' + self.name + ': ' + self.typeDecl(workaround3325, False)
        if self.isDirectFlags():
            decl += ' align(4)'
        if self.defaultValue:
            decl += ' = ' + self.defaultValue
        else:
            decl += ' = undefined'
        return decl
    

class ZigGeneratorOptions(GeneratorOptions):
    """ZigGeneratorOptions - subclass of GeneratorOptions.

    Adds options used by ZigOutputGenerator objects during Zig language header
    generation."""

    def __init__(self,
                 prefixText="",
                 indentFuncProto=True,
                 indentFuncPointer=False,
                 coreFile=None,
                 workaround3325 = False,
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
        
        self.workaround3325 = workaround3325
        """True if the generator should work around Zig-3325 by making all pointers nullable"""

        if coreFile == self.filename: coreFile = None
        self.coreFile = coreFile
        """Set to the file name that this file should include for core, or None if this is the core"""
        

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
        self.pointer_value_types = {
            'VkCString': True,
            'HANDLE': True,
            'HINSTANCE': True,
            'HWND': True,
            'HMONITOR': True,
            'LPCWSTR': True,
        }
        self.handle_types = {}
        self.resultAliases = {}


    def beginFile(self, genOpts):
        OutputGenerator.beginFile(self, genOpts)
        # Zig-specific

        # User-supplied prefix text, if any (list of strings)
        if genOpts.prefixText:
            for s in genOpts.prefixText:
                # convert to single-line comments
                if len(s) >= 2:
                    s = '//' + s[2:]
                write(s, file=self.outFile)
        
        write('const assert = @import("std").debug.assert;', file=self.outFile)
        if genOpts.coreFile:
            write('usingnamespace @import("' + genOpts.coreFile + '");', file=self.outFile)
        else:
            write("""pub const CString = [*:0]const u8;
pub fn FlagsMixin(comptime FlagType: type) type {
    comptime assert(@sizeOf(FlagType) == 4);
    return struct {
        pub const IntType = Flags;
        pub fn toInt(self: FlagType) Flags {
            return @bitCast(Flags, self);
        }
        pub fn fromInt(value: Flags) FlagType {
            return @bitCast(FlagType, value);
        }
        pub fn with(a: FlagType, b: FlagType) FlagType {
            return fromInt(toInt(a) | toInt(b));
        }
        pub fn only(a: FlagType, b: FlagType) FlagType {
            return fromInt(toInt(a) & toInt(b));
        }
        pub fn without(a: FlagType, b: FlagType) FlagType {
            return fromInt(toInt(a) & ~toInt(b));
        }
        pub fn hasAllSet(a: FlagType, b: FlagType) bool {
            return (toInt(a) & toInt(b)) == toInt(b);
        }
        pub fn hasAnySet(a: FlagType, b: FlagType) bool {
            return (toInt(a) & toInt(b)) != 0;
        }
        pub fn isEmpty(a: FlagType) bool {
            return toInt(a) == 0;
        }
    };
}

fn Handle(comptime name: []const u8, comptime InIntType: type) type {
    return extern struct {
        pub const IntType = InIntType;

        handle: IntType = 0,

        pub inline fn isNull(self: @This()) bool {
            return self.handle == 0;
        }
    };
}

const builtin = @import("builtin");
pub const CallConv = if (builtin.os.tag == .windows)
        builtin.CallingConvention.Stdcall
    else if (builtin.abi == .android and (builtin.cpu.arch.isARM() or builtin.cpu.arch.isThumb()) and builtin.Target.arm.featureSetHas(builtin.cpu.features, .has_v7) and builtin.cpu.arch.ptrBitWidth() == 32)
        // On Android 32-bit ARM targets, Vulkan functions use the "hardfloat"
        // calling convention, i.e. float parameters are passed in registers. This
        // is true even if the rest of the application passes floats on the stack,
        // as it does by default when compiling for the armeabi-v7a NDK ABI.
        builtin.CallingConvention.AAPCSVFP
    else
        builtin.CallingConvention.C;
""", file=self.outFile)

    def endFile(self):
        #write('\ntest "Compile All" { @setEvalBranchQuota(10000); _ = @import("std").meta.refAllDecls(@This()); }', file=self.outFile)
        OutputGenerator.endFile(self)

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
                        self.writeSection(section)
                    self.writeSection('command')
                    self.writeSection('commandWrapper')
        # Finish processing in superclass
        OutputGenerator.endFeature(self)
    
    def writeSection(self, name):
        contents = self.sections[name]
        if contents:
            wasNewline = True
            for str in contents:
                isNewline = '\n' in str
                if isNewline and not wasNewline:
                    self.newline()
                write(str, file=self.outFile)
                if isNewline: self.newline()
                wasNewline = isNewline
            if not wasNewline:
                self.newline()
        

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
        elif category == 'funcpointer':
            self.genFuncPointer(typeinfo, name, section)
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
            rawType = typeElem.find('type').text
            zigType = valueTypeToZigType(typeName)
            if rawType == 'VK_DEFINE_NON_DISPATCHABLE_HANDLE':
                body = 'pub const '+zigType+' = Handle("'+zigType+'", u64);'
            else:
                body = 'pub const '+zigType+' = Handle("'+zigType+'", usize);'
            self.handle_types[typeName] = True
        elif category == 'include':
            pass # don't generate includes
        else:
            body = noneStr(typeElem.text)
            for elem in typeElem:
                body += noneStr(elem.text) + noneStr(elem.tail)

        if body:
            self.appendSection(section, body)

    def genFuncPointer(self, typeinfo, name, section):
        typeElem = typeinfo.elem
        # generate the full typedef text
        text = noneStr(typeElem.text)
        for elem in typeElem:
            if elem.tag == 'name':
                preNameText = text
            text += noneStr(elem.text) + noneStr(elem.tail)
            
        # parse out the return type
        returnText = preNameText.split('(')[0]
        returnText = returnText.replace('typedef', '').strip()
        # turn it into a variable declaration and parse it
        if returnText == 'void':
            returnType = 'void'
        else:
            returnType = self.parseParam(returnText + ' retVal', True).typeDecl(self.genOpts.workaround3325, True)
        
        # parse out the parameter list
        paramStart = text.rindex('(')
        paramEnd = text.rindex(')')
        paramStrings = text[paramStart+1 : paramEnd].split(',')
        params = [self.parseParam(x, True) for x in paramStrings if x != 'void']
        
        body = 'pub const ' + valueTypeToZigType(name) + ' = fn ('
        if len(params) > 1:
            body += '\n'
            for param in params:
                body += '    ' + param.typeDecl(self.genOpts.workaround3325, True) + ',\n'
        else:
            for param in params:
                body += param.typeDecl(self.genOpts.workaround3325, True)
        
        body += ') callconv(CallConv) ' + returnType + ';'
        
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
            paramText = noneStr(member.text)
            for elem in member:
                if elem.text: paramText += ' ' + elem.text
                if elem.tail: paramText += ' ' + elem.tail

            param = self.parseParam(paramText, False, member, allMembers, typeName)
            body += '    ' + param.structDecl(self.genOpts.workaround3325) + ',\n'
        
        body += '};'

        self.appendSection('struct', body)

        
    def parseParam(self, text, isFunctionParam, param=None, allParams=[], enclosingType=None):
        parts = re.sub(r'(\S)\*', r'\1 *', re.sub(r'\*(\S)', r'* \1', re.sub(r'\]', r'] ', text))).split()
        parts = [x for x in parts if x != 'struct']

        # parts is of the form
        # (const)? <valueType> ((const)? \*)* <name> (\[ <bufferLen> \])*      
        indirects = []

        while parts[-1].endswith(']'):
            if parts[-1].startswith('['):
                fullDecl = parts.pop()
                length = fullDecl[1:-1].strip()
            else:
                parts.pop()
                length = parts.pop()
                parts.pop()

            isNullTerminated = length in nullTerminatedSizes
            indirects.append(ZigIndirect(ZigIndirect.TYPE_BUFFER, length, False, False, isNullTerminated))
        
        if parts[0] == 'const':
            parts[0] = parts[1]
            parts[1] = 'const'
            # check for redundant const (e.g. const char const * const)
            if len(parts) > 2 and parts[2] == 'const':
                parts.pop(2)

        name = parts.pop()
        cValueType = parts.pop(0)
        parts.reverse()
        
        # fix name collisions with zig keywords
        if name == 'type':
            name = 'inType'
        
        numStars = 0
        for part in parts:
            if part == '*':
                numStars += 1

        if param is not None:
            lengths = noneStr(param.get('len')).split(',')
            if len(lengths) == 1 and len(lengths[0]) == 0: lengths = []
            while len(lengths) > numStars: lengths.pop()
            while len(lengths) < numStars: lengths.append('1')
            isParamOptional = param.get('optional') == 'true'
        else:
            lengths = ["_" for x in range(numStars)]
            isParamOptional = True
            if cValueType == 'char' and numStars > 0:
                lengths[-1] = 'null-terminated'
            
        if cValueType == 'char' and len(parts) >= 2 and parts[-1] == 'const' and parts[-2] == '*' and lengths[-1] == 'null-terminated':
            lengths.pop()
            parts.pop()
            parts.pop()
            cValueType = 'VkCString'
    
        # IMPORTANT: We have only processed buffers up to this point.
        if isFunctionParam and len(indirects) > 0:
            # fix up fixed-length buffer params: const float[4] becomes *const[4]f32
            isConst = len(parts) > 0 and parts[0] == 'const'
            if isConst: parts.pop(0)

            extraPointer = ZigIndirect(ZigIndirect.TYPE_POINTER, '1', isParamOptional, isConst)
            indirects.insert(0, extraPointer)

        starIndex = 0
        for part in parts:
            if part == '*':
                isOptional = len(indirects) == 0 and (isParamOptional or cValueType == 'void')
                length = lengths[starIndex]
                starIndex += 1
                # set isConst to false, we will set it after if the next token is 'const'
                indirects.append(ZigIndirect(ZigIndirect.TYPE_POINTER, length, isOptional, False))

            elif part == 'const':
                indirects[-1].isConst = True

        valueTypeIsPointer = self.isValueTypePointer(cValueType)
        valueTypeOptional = isParamOptional and len(indirects) == 0 and valueTypeIsPointer

        if cValueType == enclosingType:
            declValueType = '@This()'
        else:
            declValueType = valueTypeToZigType(cValueType, fixFlagType=True)
        
        if len(indirects) > 0:
            isPointer = indirects[0].type == ZigIndirect.TYPE_POINTER
        else:
            isPointer = valueTypeIsPointer
            
        if valueTypeIsPointer:
            metaType = ZigValueType.TYPE_POINTER
        elif cValueType in self.handle_types:
            metaType = ZigValueType.TYPE_HANDLE
        elif 'Flags' in declValueType:
            metaType = ZigValueType.TYPE_FLAGS
        else:
            metaType = ZigValueType.TYPE_OTHER

        defaultValue = None
        if isParamOptional:
            if isPointer:
                defaultValue = 'null'
            elif metaType == ZigValueType.TYPE_HANDLE:
                defaultValue = declValueType + '{}'
            elif metaType == ZigValueType.TYPE_FLAGS:
                defaultValue = declValueType + '{}'
            else:
                defaultValue = '0'
        elif name == 'pNext' and cValueType == 'void':
            defaultValue = 'null'
        elif allParams and len(lengths) >= 1:
            parentElem = None
            for m in allParams:
                mName = m.find('name').text
                if mName == lengths[0]:
                    parentElem = m
                    break
            # determine if the parent is optional
            if parentElem and parentElem.get('optional') == 'true':
                defaultValue = 'undefined'
        else:
            valuesStr = param.get('values')
            if valuesStr:
                values = valuesStr.split(',')
                if len(values) == 1 and values[0].startswith('VK_STRUCTURE_TYPE_'):
                    defaultValue = values[0].replace('VK_STRUCTURE_TYPE_', '.')
        
        valueType = ZigValueType(cValueType, declValueType, metaType, valueTypeOptional)
        return ZigParam(name, valueType, indirects, defaultValue, isParamOptional)

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
            if 'FlagBits' in alias:
                return # don't generate FlagBits aliases since we don't need those types in Zig
            # If the group name is aliased, just emit a typedef declaration
            # for the alias.
            body = 'pub const ' + valueTypeToZigType(groupName) + ' = ' + valueTypeToZigType(alias) + ';'
            self.appendSection(section, body)
        else:
            (section, body) = self.buildEnumZigDecl(groupinfo, groupName)
            self.appendSection(section, body)

    def genEnum(self, enuminfo, name, alias):
        """Generate enumerants.

        <enum> tags may specify their values in several ways, but are usually
        just integers."""
        OutputGenerator.genEnum(self, enuminfo, name, alias)
        (_, strVal) = self.enumToValue(enuminfo.elem, False)
        if strVal.startswith('VK_'): strVal = strVal[3:]
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
            body = 'pub const ' + valueTypeToZigType(name) + ' = packed struct {\n    __reserved_bits_00_31: u32 = 0,\n    pub usingnamespace FlagsMixin(@This());\n};'
        
        if body:
            self.appendSection("bitmask", body)
            

    def buildEnumZigDecl_Bitmask(self, groupinfo, groupName):
        """Generate the Zig declaration for an "enum" that is actually a
        set of flag bits"""
        groupElem = groupinfo.elem
        flagBitsName = groupinfo.elem.get('name')
        flagTypeName = flagBitsName.replace('FlagBits', 'Flags')
        rootName = flagBitsName.replace('FlagBits', '')

        (prefix, suffix) = splitTypeName(rootName)
        expandPrefix = re.sub(r'([0-9a-z_])([A-Z0-9])', r'\1_\2', prefix).upper() + '_'
        expandSuffix = '_' + suffix

        zigFlagTypeName = valueTypeToZigType(flagTypeName)

        # Prefix
        body = "pub const " + zigFlagTypeName + " = packed struct {\n"

        declValues = [None for _ in range(32)];
        aliasBody = ''
        usedNames = {}
        needSelf = False

        # Loop over the nested 'enum' tags.
        for elem in groupElem.findall('enum'):
            # Convert the value to an integer and use that to track min/max.
            # Values of form -(number) are accepted but nothing more complex.
            # Should catch exceptions here for more complex constructs. Not yet.
            (numVal, strVal) = self.enumToValue(elem, True)
            name = flagNameToZigName(elem.get('name'), expandPrefix, expandSuffix)
            # some enums incorrectly have duplicated names :(
            if name in usedNames: continue
            usedNames[name] = True

            if numVal is not None:
                if numVal != 0 and (numVal & (numVal-1)) == 0:
                    index = bitIndex(numVal);
                    if declValues[index]:
                        # this bit is already in use, make this an alias
                        aliasBody += "    pub const %s = fromInt(%s);\n" % (name, strVal)
                    else:
                        declValues[index] = name;
                else:
                    aliasBody += "    pub const %s = fromInt(%s);\n" % (name, strVal)
            else:
                # this is an alias
                strVal = flagNameToZigName(strVal, expandPrefix, expandSuffix)
                aliasBody += '    pub const %s = Self{ .%s = true };\n' % (name, strVal)
                needSelf = True

        declBody = '';
        for index, flag in enumerate(declValues):
            if flag:
                declBody += '    ' + flag + ': bool = false,\n'
            else:
                declBody += '    __reserved_bit_%02d: bool = false,\n' % index

        if declBody:
            body += declBody
            body += '\n'

        if needSelf:
            body += '    const Self = @This();\n'

        if aliasBody:
            body += aliasBody
            body += '\n'
        
        if needSelf:
            body += '    pub usingnamespace FlagsMixin(Self);\n'
        else:
            body += '    pub usingnamespace FlagsMixin(@This());\n'

        # Postfix
        body += '};'

        return ("bitmask", body)

    def buildEnumZigDecl_Enum(self, groupinfo, groupName):
        """Generate the Zig declaration for an enumerated type"""
        groupElem = groupinfo.elem

        # Break the group name into prefix and suffix portions for range
        (prefix, suffix) = splitTypeName(groupName)
        expandPrefix = re.sub(r'([0-9a-z_])([A-Z0-9])', r'\1_\2', prefix).upper() + '_'
        expandSuffix = '_' + suffix

        # Prefix
        body = ["pub const %s = extern enum(i32) {" % valueTypeToZigType(groupName)]

        # Get a list of nested 'enum' tags.
        enums = groupElem.findall('enum')

        # Check for and report duplicates, and return a list with them
        # removed.
        enums = self.checkDuplicateEnums(enums)

        # Accumulate non-numeric enumerant values separately and append
        # them following the numeric values, to allow for aliases.
        aliasText = []
        usedNames = {}

        for elem in enums:
            # Convert the value to an integer and use that to track min/max.
            # Values of form -(number) are accepted but nothing more complex.
            # Should catch exceptions here for more complex constructs. Not yet.
            (numVal, strVal) = self.enumToValue(elem, True)
            name = enumNameToZigName(elem.get('name'), expandPrefix, expandSuffix)
            # some enums incorrectly have duplicated names :(
            if name in usedNames: continue
            usedNames[name] = True

            # Extension enumerants are only included if they are required
            if self.isEnumRequired(elem):
                if numVal is not None:
                    decl = "    {} = {},".format(name, strVal)
                    body.append(decl)
                else:
                    if groupName == 'VkResult':
                        self.resultAliases[elem.get('name')] = strVal
                    strVal = enumNameToZigName(strVal, expandPrefix, expandSuffix)
                    decl = "    pub const {} = Self.{};".format(name, strVal)
                    aliasText.append(decl)

        # Make the enum extensible
        body.append('    _,')

        # Now append the non-numeric enumerant values
        if aliasText:
            body.append('')
            body.append('    const Self = @This();')
            body.extend(aliasText)

        # Postfix
        body.append("};")

        return ('group', '\n'.join(body))

    def genCmd(self, cmdinfo, name, alias):
        "Command generation"
        OutputGenerator.genCmd(self, cmdinfo, name, alias)

        decls = self.makeZigDecls(cmdinfo.elem, name)
        self.appendSection('command', decls[0])
        self.appendSection('commandWrapper', decls[1])

    def makeZigDecls(self, cmd, name):
        """Return Zig prototype and function pointer typedef for a
        `<command>` Element, as a two-element list of strings.

        - cmd - Element containing a `<command>` tag"""
        proto = cmd.find('proto')
        paramTags = cmd.findall('param')
        # Begin accumulating prototype and typedef strings

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
                commandName = text
            else:
                returnType += text + tail

        returnType = returnType.strip()
        zigReturnType = returnType
        if returnType != 'void':
            zigReturnType = valueTypeToZigType(returnType, fixFlagType=True)

        params = []
        for param in paramTags:
            paramText = noneStr(param.text)
            for tag in param:
                paramText += ' ' + noneStr(tag.text)
                paramText += ' ' + noneStr(tag.tail)
            params.append(self.parseParam(paramText, True, param, paramTags))

        # Now add the parameter declaration list, which is identical
        # for prototypes and typedefs. Concatenate all the text from
        # a <param> node without the tags. No tree walking required
        # since all tags are ignored.

        externFn = 'pub extern fn ' + commandName
        # Indented parameters
        if len(params) > 1:
            externFn += '(\n'
            for p in params:
                externFn += '    ' + p.paramDecl(self.genOpts.workaround3325, True) + ',\n'
            externFn += ')'
        else:
            externFn += '('
            for p in params:
                externFn += p.paramDecl(self.genOpts.workaround3325, True)
            externFn += ')'
            
        externFn += ' callconv(CallConv) ' + zigReturnType + ';'

        # Figure out the wrapper function
        ZigParam.link(params)

        funcDecl = ''
        variants = ['']
        index = 0
        while index < len(variants):
            variant = variants[index]
            index += 1

            # Start with the return type.  It is one of four things:
            # VkResult with multiple success codes (return result)
            # VkResult with only error codes
            # void
            # other

            # bool, whether to return the returned value
            forwardReturn = False
            # list of ZigParam, parameters which are out values
            returnParams = []
            # list of string, assert conditions
            asserts = []
            # list of ZigParam, local declarations
            locals = []
            # list of string, cleanup lines of zig code
            cleanup = []
            # list of ZigParam, exposed params
            userParams = []
            # list of string, passed parameter values
            apiParams = []
            # list of string, lines of error handler code
            errorHandler = []
            # list of string, zig error names
            returnErrors = []
            # list of string, C VkResult values that indicate success
            successCodes = []
            
            shouldWrap = False

            if returnType == 'VkResult':
                shouldWrap = True
                successCodes = cmd.get('successcodes')
                if successCodes is None: successCodes = []
                else: successCodes = [x for x in successCodes.split(',') if x]
                
                errorCodes = cmd.get('errorcodes')
                if errorCodes is None: errorCodes = []
                else: errorCodes = [x for x in errorCodes.split(',') if x]
                
                undocumentedError = 'VK_UNDOCUMENTED_ERROR'
                # TODO: When Zig implements @expect, declare this as unexpected to reduce overhead
                errorHandler.append('    if (@bitCast(c_int, result) < 0) {')
                if errorCodes:
                    errorHandler.append('        return switch (result) {')
                    for err in errorCodes:
                        while err in self.resultAliases:
                            err = self.resultAliases[err]
                        zigError = err.replace('ERROR_', '')
                        zigEnum = err.replace('VK_', '')
                        returnErrors.append(zigError)
                        errorHandler.append('            .'+zigEnum+' => error.'+zigError+',')
                    errorHandler.append('            else => error.' + undocumentedError + ',')
                    errorHandler.append('        };')
                else:
                    errorHandler.append('        return error.' + undocumentedError + ';')
                returnErrors.append(undocumentedError)
                errorHandler.append('    }')
                
                if variant == 'Count':
                    successCodes.remove('VK_INCOMPLETE')

                apiReturnLocation = 'const result'
                if len(successCodes) > 1:
                    forwardReturn = True
            elif returnType == 'void':
                apiReturnLocation = None
            else:
                apiReturnLocation = 'returnValues.result'
                forwardReturn = True

            for param in params:
                if param.isConstBufferLen():
                    shouldWrap = True
                    buffer = param.buffers[0].bufferName()
                    apiParams.append('@intCast('+param.valueType.declValueType+', '+buffer+'.len)')
                elif param.isMutableBufferLen():
                    shouldWrap = True
                    buffer = param.buffers[0].bufferName()
                    valueParam = param.getValueParam()
                    if variant == 'Count':
                        returnParams.append(valueParam)
                        apiParams.append('&returnValues.' + valueParam.name)
                    else:
                        valueParam.defaultValue = '@intCast('+valueParam.valueType.declValueType+', '+buffer+'.len)'
                        locals.append(valueParam)
                        apiParams.append('&' + valueParam.name)
                elif param.isBuffer():
                    shouldWrap = True
                    sliceParam = param.getBufferParam()
                    
                    if param.indirections[0].lenParam is not None:
                        parent = param.indirections[0].lenParam
                        generateUserParam = True
                        if parent.isMutableBufferLen() and param.isOptional:
                            if variant == 'Count':
                                apiParams.append('null')
                                generateUserParam = False
                            else:
                                returnParams.append(sliceParam.getCopy())
                                cleanup.append('returnValues.'+sliceParam.name+' = '+sliceParam.name+'[0..'+parent.bufferName()+'];')
                                if variant == '' and not ('Count' in variants):
                                    variants.append('Count')

                        if generateUserParam:
                            userParams.append(sliceParam)
                            apiParams.append(sliceParam.name+'.ptr')
                            if param != parent.buffers[0]:
                                lenBuffer = parent.buffers[0].bufferName()
                                asserts.append(sliceParam.name+'.len >= '+lenBuffer+'.len')
                            
                    elif '::' in param.indirections[0].cLength:
                        lengthParts = param.indirections[0].cLength.split('::')
                        parentParam = None
                        for p in params:
                            if p.name == lengthParts[0]:
                                parentParam = p
                                break
                        if parentParam is not None:
                            if parentParam.isValueInParam():
                                lengthParts[0] = parentParam.bufferName()
                            asserts.append(sliceParam.name + '.len >= ' + '.'.join(lengthParts))
                        userParams.append(sliceParam)
                        apiParams.append(sliceParam.name+'.ptr')
                    else:
                        userParams.append(sliceParam)
                        apiParams.append(sliceParam.name+'.ptr')
                
                elif param.isOutParam():
                    shouldWrap = True
                    valueParam = param.getValueParam()
                    returnParams.append(valueParam)
                    apiParams.append('&returnValues.'+valueParam.name)
                elif param.isValueInParam():
                    shouldWrap = True
                    valueParam = param.getValueParam()
                    userParams.append(valueParam)
                    apiParams.append('&'+valueParam.name)
                elif param.isDirectFlags():
                    shouldWrap = True
                    userParams.append(param)
                    apiParams.append(param.name+'.toInt()')
                elif param.isDirectHandle():
                    shouldWrap = True
                    userParams.append(param)
                    apiParams.append(param.name+'.handle')
                else:
                    userParams.append(param)
                    apiParams.append(param.name)
                    
            if not shouldWrap:
                funcDecl = 'pub const ' + name[2:] + ' = ' + name + ';'
            else:                
                # set wrapperReturnType, returnStatement
                numReturns = len(returnParams)
                if forwardReturn: numReturns += 1
                if numReturns >= 2:
                    # generate a result struct
                    wrapperReturnType = name[2:]+'Result'
                    funcDecl += 'pub const '+wrapperReturnType+' = struct {\n'
                    if forwardReturn:
                        funcDecl += '    result: '+zigReturnType+',\n'
                    for param in returnParams:
                        funcDecl += '    ' + param.structDecl(self.genOpts.workaround3325) + ',\n'
                    funcDecl += '};\n'
                    locals.insert(0, ZigParam('returnValues', ZigValueType('', wrapperReturnType, False, False), [], 'undefined', False)) 
                    if len(successCodes) > 1:
                        cleanup.append('returnValues.result = result;')
                    returnStatement = 'return returnValues;'
                elif forwardReturn:
                    wrapperReturnType = zigReturnType
                    apiReturnLocation = 'const result'
                    returnStatement = 'return result;'
                elif numReturns == 1:
                    returnVal = returnParams[0]
                    returnVal.name = 'out_' + returnVal.name
                    locals.insert(0, returnVal)
                    wrapperReturnType = returnVal.typeDecl(self.genOpts.workaround3325, False)
                    returnStatement = 'return '+returnVal.name+';'

                    apiParams = [x.replace('returnValues.', 'out_') for x in apiParams]
                    cleanup = [x.replace('returnValues.', 'out_') for x in cleanup]
                else:
                    wrapperReturnType = 'void'
                    returnStatement = None
                    
                if returnErrors:
                    wrapperReturnType = 'error{' + ','.join(returnErrors) + '}!' + wrapperReturnType
                
                (prefix, suffix) = splitTypeName(name[2:])
                funcDecl += 'pub inline fn '+prefix+variant+suffix+'('
                funcDecl += ', '.join(p.paramDecl(self.genOpts.workaround3325, False) for p in userParams)
                funcDecl += ') ' + wrapperReturnType + ' {\n'
                for local in locals:
                    funcDecl += '    ' + local.varDecl(self.genOpts.workaround3325) + ';\n'

                for cond in asserts:
                    funcDecl += '    assert(' + cond + ');\n'

                funcDecl += '    '
                if apiReturnLocation:
                    funcDecl += apiReturnLocation + ' = '
                funcDecl += name + '(' + ', '.join(apiParams) + ');\n'
                
                if errorHandler:
                    funcDecl += '\n'.join(errorHandler) + '\n'
                
                for line in cleanup:
                    funcDecl += '    ' + line + '\n'
                
                if returnStatement:
                    funcDecl += '    ' + returnStatement + '\n'
                    
                funcDecl += '}\n'
        
        if funcDecl and funcDecl[-1] == '\n': funcDecl = funcDecl[:-1]
        return [externFn, funcDecl]
        
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
            if value[0] != '"':
                value = value.replace("0ULL", "@as(u64, 0)").replace("0U", "@as(u32, 0)")
                if value[-1] == 'f':
                    value = '@as(f32, ' + value[:-1] + ')'
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
        return typeName.startswith('PFN_') or typeName in self.pointer_value_types
