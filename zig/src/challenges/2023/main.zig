const challenge1 = @import("challenge1/main.zig");
const challenge2 = @import("challenge2/main.zig");
// Import other challenges as needed

pub fn runAllChallenges() !void {
    try challenge1.run();
    try challenge2.run();
    // Call other challenges in sequence
}

pub fn runChallenge(challengeNumber: u8) !void {
    switch (challengeNumber) {
        1 => try challenge1.run(),
        2 => try challenge2.run(),
        else => return error.ChallengeNotFound,
    }
}
