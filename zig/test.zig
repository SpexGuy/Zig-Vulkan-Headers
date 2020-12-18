const std = @import("std");
const vk = @import("vulkan_core.zig");

fn refAllDeclsRecursive(comptime T: type) void {
    comptime {
        for (std.meta.declarations(T)) |decl| {
            _ = decl;
            if (decl.is_pub and decl.data == .Type) {
                const info = @typeInfo(decl.data.Type);
                if (info == .Struct or info == .Union or info == .Opaque or info == .Enum) {
                    refAllDeclsRecursive(decl.data.Type);
                }
            }
        }
    }
}

test "Compile All" {
    @setEvalBranchQuota(100000);
    refAllDeclsRecursive(vk);
}
