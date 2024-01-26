const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn main() !void {
    // try challenge1("../../../../../data/year2023/challenge1/puzzle.txt");
    try challenge2("../../../../../data/year2023/challenge1/puzzle.txt");
}

pub fn challenge1(file_path: []const u8) !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();
    const buff = try read_from_challenge(allocator, file_path);
    defer allocator.free(buff);
    var iter = std.mem.splitSequence(u8, buff, "\n");
    var total: u64 = 0;

    var count: usize = 0;
    while (iter.next()) |line| : (count += 1) {
        std.debug.print("{d:>2}: {s}\n", .{ count, line });
        var bucket: [2]?u8 = .{ null, null };
        for (0..line.len) |i| {
            // std.debug.print("i: {d}\n", .{i});
            const front_element = line[i];
            const back_element = line[line.len - 1 - i];

            if (bucket[0] == null and std.ascii.isDigit(front_element)) {
                bucket[0] = front_element - '0';
            }

            if (bucket[1] == null and std.ascii.isDigit(back_element)) {
                bucket[1] = back_element - '0';
            }

            if (bucket[0] != null and bucket[1] != null) {
                break;
            }
        }
        const value0 = bucket[0] orelse 0;
        const value1 = bucket[1] orelse 0;
        total += (value0 * 10) + value1;
        for (0..bucket.len) |i| {
            bucket[i] = null;
        }
    }
    std.debug.print("\ntotal: {d}\n", .{total});
}

fn createMap(allocator: Allocator) !std.StringHashMap(u8) {
    var my_hash_map = std.StringHashMap(u8).init(allocator);
    try my_hash_map.put("zero", 0);
    try my_hash_map.put("one", 1);
    try my_hash_map.put("three", 3);
    try my_hash_map.put("two", 2);
    try my_hash_map.put("four", 4);
    try my_hash_map.put("five", 5);
    try my_hash_map.put("six", 6);
    try my_hash_map.put("seven", 7);
    try my_hash_map.put("eight", 8);
    try my_hash_map.put("nine", 9);
    return my_hash_map;
}

fn containsAtN(allocator: Allocator, my_hash_map: std.StringHashMap(u8), string: std.ArrayList(u8), number: usize, i: usize, rev: bool) !?u8 {
    if (string.items.len >= number) {
        // std.debug.print("{d}, {d}\n", .{ i, number });
        const mod: usize = i - number;
        // const strSlice = string.items[0..length];
        if (rev) {
            var nString: []u8 = try allocator.alloc(u8, number);
            defer allocator.free(nString);
            const sliceLength: usize = string.items.len;
            var k: usize = 0;
            while (k < number) {
                // std.debug.print("{}, {}, {any}\n", .{ sliceLength, sliceLength - 1 - k, nString });
                nString[k] = string.items[sliceLength - 1 - k];
                k += 1;
            }
            return my_hash_map.get(nString);
            // std.debug.print("{any}", .{nString});
        } else {
            // nString = strSlice[mod .. number - 1 + mod];
            const nString = string.items[mod .. number + mod];
            return my_hash_map.get(nString);
        }
    }
    return null;
}

pub fn challenge2(file_path: []const u8) !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();
    const buff = try read_from_challenge(allocator, file_path);
    defer allocator.free(buff);
    var iter = std.mem.splitSequence(u8, buff, "\n");
    var total: u64 = 0;
    var my_hash_map = try createMap(allocator);
    defer my_hash_map.deinit();

    var count: usize = 0;
    while (iter.next()) |line| : (count += 1) {
        // std.debug.print("{d:>2}: {s}\n", .{ count, line });
        var bucket: [2]?u8 = .{ null, null };
        var frontStringBuilder = std.ArrayList(u8).init(allocator);
        defer frontStringBuilder.deinit(); // Clean up the memory when done
        var backStringBuilder = std.ArrayList(u8).init(allocator);
        defer backStringBuilder.deinit(); // Clean up the memory when done
        for (0..line.len) |i| {
            // std.debug.print("i: {d}\n", .{i});
            const front_element = line[i];
            const back_element = line[line.len - 1 - i];

            if (bucket[0] == null) {
                if (std.ascii.isDigit(front_element)) {
                    bucket[0] = front_element - '0';
                } else {
                    try frontStringBuilder.append(front_element);
                    // std.debug.print("frontStringBuilder: {any}\n", .{frontStringBuilder});
                    for (3..6) |j| {
                        if (frontStringBuilder.items.len < j) {
                            break;
                        }
                        const value = try containsAtN(allocator, my_hash_map, frontStringBuilder, j, i + 1, false);
                        if (value != null) {
                            bucket[0] = value;
                            break;
                        }
                    }
                }
            }

            if (bucket[1] == null) {
                if (std.ascii.isDigit(back_element)) {
                    bucket[1] = back_element - '0';
                } else {
                    try backStringBuilder.append(back_element);
                    // std.debug.print("backStringBuilder: {any}\n\n", .{backStringBuilder});
                    for (3..6) |j| {
                        const value = try containsAtN(allocator, my_hash_map, backStringBuilder, j, i + 1, true);
                        if (value != null) {
                            bucket[1] = value;
                            break;
                        }
                    }
                }
            }

            if (bucket[0] != null and bucket[1] != null) {
                break;
            }
        }
        const value0 = bucket[0] orelse 0;
        const value1 = bucket[1] orelse 0;
        total += (value0 * 10) + value1;
        std.debug.print("\ncurrent total: {d}\n", .{total});
        for (0..bucket.len) |i| {
            bucket[i] = null;
        }
    }
    std.debug.print("\ntotal: {d}\n", .{total});
}

pub fn read_from_challenge(allocator: std.mem.Allocator, file_path: []const u8) ![]u8 {
    var path_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fs.realpath(file_path, &path_buffer);

    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const buffer_size = 200000;
    const file_buffer = try file.readToEndAlloc(allocator, buffer_size);

    // const iter = std.mem.splitSequence(u8, file_buffer, "\n");

    return file_buffer;
}
