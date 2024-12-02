const std = @import("std");
const util = @import("../../../util.zig");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;

pub fn run() !void {
    std.log.info("Zig version: {}", .{builtin.zig_version});
    try challenge1("../data/year2023/challenge1/puzzle.txt");
    try challenge2("../data/year2023/challenge1/puzzle.txt");
}

pub fn challenge1(file_path: []const u8) !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();
    const buff = try util.read_from_challenge(allocator, file_path);
    defer allocator.free(buff);
    var iter = std.mem.splitSequence(u8, buff, "\n");
    var total: u64 = 0;

    var count: usize = 0;
    while (iter.next()) |line| : (count += 1) {
        var bucket: [2]?u8 = .{ null, null };
        for (0..line.len) |i| {
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
    std.debug.print("\nYear 2023 challenge 1: {d}\n", .{total});
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

fn containsAtN(allocator: Allocator, my_hash_map: std.StringHashMap(u8), string: *std.ArrayList(u8), number: usize, i: usize, rev: bool) !?u8 {
    if (string.items.len >= number) {
        const mod: usize = i - number;
        if (rev) {
            var nString: []u8 = try allocator.alloc(u8, number);
            defer allocator.free(nString);
            const sliceLength: usize = string.items.len;
            var k: usize = 0;
            while (k < number) {
                nString[k] = string.items[sliceLength - 1 - k];
                k += 1;
            }
            return my_hash_map.get(nString);
        } else {
            const nString = string.items[mod .. number + mod];
            return my_hash_map.get(nString);
        }
    }
    return null;
}

fn processChar(allocator: std.mem.Allocator, numberMap: std.StringHashMap(u8), stringBuilder: *std.ArrayList(u8), charPointer: u8, bucket: *?u8, index: usize, reverse: bool) !void {
    if (std.ascii.isDigit(charPointer)) {
        bucket.* = charPointer - '0';
    } else {
        try stringBuilder.append(charPointer);
        for (3..6) |j| {
            if (stringBuilder.items.len < j) {
                break;
            }
            const value = try containsAtN(allocator, numberMap, stringBuilder, j, index + 1, reverse);
            if (value) |v| {
                bucket.* = v;
                break;
            }
        }
    }
}

pub fn challenge2(file_path: []const u8) !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();
    const buff = try util.read_from_challenge(allocator, file_path);
    defer allocator.free(buff);
    var iter = std.mem.splitSequence(u8, buff, "\n");
    var numberMap = try createMap(allocator);
    defer numberMap.deinit();

    var total: u64 = 0;
    while (iter.next()) |line| {
        var bucket: [2]?u8 = .{ null, null };
        var frontStringBuilder = std.ArrayList(u8).init(allocator);
        defer frontStringBuilder.deinit();
        var backStringBuilder = std.ArrayList(u8).init(allocator);
        defer backStringBuilder.deinit();
        for (0..line.len) |char_index| {
            if (bucket[0] == null) {
                try processChar(allocator, numberMap, &frontStringBuilder, line[char_index], &bucket[0], char_index, false);
            }

            if (bucket[1] == null) {
                try processChar(allocator, numberMap, &backStringBuilder, line[line.len - 1 - char_index], &bucket[1], char_index, true);
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
    std.debug.print("\nYear 2023 challenge 1: {d}\n", .{total});
}
