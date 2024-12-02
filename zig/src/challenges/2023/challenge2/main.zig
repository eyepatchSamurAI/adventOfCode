const std = @import("std");
const util = @import("../../../util.zig");

const Allocator = std.mem.Allocator;
// 0.12.0-dev.2316+ac2930332
pub fn run() !void {
    try challenge1("../data/year2023/challenge2/puzzle.txt");
}

fn getDigit(color: []const u8, endPos: usize) u8 {
    var total: u8 = 0;
    var posIndex = endPos;
    var multiplier: u8 = 1;
    while (posIndex > 0) : (posIndex -= 1) {
        const digit: u8 = (color[posIndex - 1]) - '0';
        total += digit * multiplier;
        multiplier *= 10;
    }
    return total;
}

fn isHigher(color: []const u8) bool {
    const maxRed = 12;
    const maxGreen = 13;
    const maxBlue = 14;
    const colorLength = color.len;
    var number: usize = undefined;
    var isHigherValue: bool = undefined;
    if (color[colorLength - 3] == 'r') {
        number = getDigit(color, colorLength - 4);
        isHigherValue = number <= maxRed;
    } else if (color[colorLength - 4] == 'b') {
        number = getDigit(color, colorLength - 5);
        isHigherValue = number <= maxBlue;
    } else if (color[colorLength - 5] == 'g') {
        number = getDigit(color, colorLength - 6);
        isHigherValue = number <= maxGreen;
    }
    return isHigherValue;
}

pub fn challenge1(file_path: []const u8) !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();
    const buff = try util.read_from_challenge(allocator, file_path);
    defer allocator.free(buff);
    var iter = std.mem.splitSequence(u8, buff, "\n");
    var impossibleGames = std.ArrayList(usize).init(allocator);
    defer impossibleGames.deinit();
    const startingIndex = 8;
    var gameIndex: usize = 1;
    while (iter.next()) |line| : (gameIndex += 1) {
        var moddedLine: []const u8 = undefined;
        if (gameIndex < 10) {
            moddedLine = line[startingIndex..line.len];
        } else if (gameIndex >= 10 and gameIndex < 100) {
            moddedLine = line[(startingIndex + 1)..line.len];
        } else if (gameIndex >= 100) {
            moddedLine = line[(startingIndex + 2)..line.len];
        }
        var splitIter = std.mem.splitSequence(u8, moddedLine, ";");

        var matchIndex: u8 = 1;
        while (splitIter.next()) |colorData| : (matchIndex += 1) {
            var moddedColorData: []const u8 = undefined;
            if (matchIndex == 1) {
                moddedColorData = colorData[0..colorData.len];
            } else {
                moddedColorData = colorData[1..colorData.len];
            }

            var splitColorData = std.mem.splitSequence(u8, moddedColorData, ",");
            var colorDataIndex: usize = 0;
            var gameWasImpossible = false;

            while (splitColorData.next()) |color| : (colorDataIndex += 1) {
                var moddedColor: []const u8 = undefined;
                if (colorDataIndex == 0) {
                    moddedColor = color[0..color.len];
                } else {
                    moddedColor = color[1..color.len];
                }
                const isGamePossible = isHigher(moddedColor);
                if (!isGamePossible) {
                    gameWasImpossible = true;
                    break;
                }
            }
            if (gameWasImpossible) {
                try impossibleGames.append(gameIndex);
                break;
            }
        }
    }
    var total: usize = 0;
    for (impossibleGames.items) |item| {
        total += item;
    }
    const gameIdSum: usize = ((gameIndex - 1) * ((gameIndex - 1) + 1)) / 2;
    const finalResult = gameIdSum - total;
    std.debug.print("\nYear 2023 challenge 1: {d}\n", .{finalResult});
}

pub fn challenge2(file_path: []const u8) !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();
    const buff = try util.read_from_challenge(allocator, file_path);
    defer allocator.free(buff);
    const iter = std.mem.splitSequence(u8, buff, "\n");
    _ = iter; // autofix
}
