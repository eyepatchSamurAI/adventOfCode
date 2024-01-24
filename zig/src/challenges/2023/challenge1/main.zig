const std = @import("std");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();
    const buff = try read_from_challenge(allocator);
    defer allocator.free(buff);
    var iter = std.mem.splitSequence(u8, buff, "\n");
    var total: u64 = 0;

    var count: usize = 0;
    // const buff = iter.buffer;
    while (iter.next()) |line| : (count += 1) {
        // for (iter.buffer) |line| {
        std.debug.print("{d:>2}: {s}\n", .{ count, line });
        // std.debug.print("line: {any}\n", .{170});
        var bucket: [2]?u8 = undefined;
        bucket[0] = null;
        bucket[1] = null;
        // var bucket: [2]?u8 = ?u8{ null, null };
        std.debug.print("bucket: {any}\n", .{bucket});

        // const len: u8 = line.len;
        for (0..line.len) |i| {
            std.debug.print("i: {d}\n", .{i});
            const front_element = line[i];
            const back_element = line[line.len - 1 - i];

            if (bucket[0] == null and std.ascii.isDigit(front_element)) {
                // const integer = try std.fmt.parseInt(u8, front_element, 10);
                // std.debug.print("front_element: {d}\n", .{front_element});
                bucket[0] = front_element - '0';
                // std.debug.print("front_element: {?}\n", .{bucket[0]});
            }

            if (bucket[1] == null and std.ascii.isDigit(back_element)) {
                // const integer = try std.fmt.parseInt(u8, back_element, 10);
                bucket[1] = back_element - '0';
            }

            if (bucket[0] != null and bucket[1] != null) {
                break;
            }
        }
        const value0 = bucket[0] orelse 0;
        const value1 = bucket[1] orelse 0;
        std.debug.print("values: {d} {d}\n", .{ value0, value1 });
        total += (value0 * 10) + value1;
        std.debug.print("{d}\n", .{total});
        bucket[0] = null;
        bucket[1] = null;
    }
    std.debug.print("\ntotal: {d}\n", .{total});
}

pub fn read_from_challenge(allocator: std.mem.Allocator) ![]u8 {
    var path_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fs.realpath("../../../../../data/year2023/challenge1/puzzle.txt", &path_buffer);

    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const buffer_size = 200000;
    const file_buffer = try file.readToEndAlloc(allocator, buffer_size);

    // const iter = std.mem.splitSequence(u8, file_buffer, "\n");

    return file_buffer;
}
