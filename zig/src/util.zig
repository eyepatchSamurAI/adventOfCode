const std = @import("std");

pub fn read_from_challenge(allocator: std.mem.Allocator, file_path: []const u8) ![]u8 {
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    const absolute_path = try std.fs.path.resolve(allocator, &.{
        cwd_path,
        file_path,
    });
    defer allocator.free(absolute_path);

    var path_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fs.realpath(absolute_path, &path_buffer);

    const file = try std.fs.openFileAbsolute(path, .{});
    const fileSize = (try file.stat()).size;
    defer file.close();

    const file_buffer = try file.readToEndAlloc(allocator, fileSize);

    return file_buffer;
}
