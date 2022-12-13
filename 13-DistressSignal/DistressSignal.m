// Distress Signal
// https://adventofcode.com/2022/day/13

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSString (Utility)
- (void)printLine;
@end

@implementation NSString (Utility)
- (void)printLine {
     [self writeToFile:@"/dev/stdout" atomically:NO encoding:NSUTF8StringEncoding error:NULL];
     [@"\n" writeToFile:@"/dev/stdout" atomically:NO encoding:NSUTF8StringEncoding error:NULL];
}
@end


/**
 A packet containing a list of numbers and lists.
 */
@interface Packet : NSObject

/**
 The top-level list for the packet.
 Each element is either an `NSNumber` (representing a number) or an `NSArray` (representing a list).
 */
@property (readonly) NSArray *elements;

/**
 Initializes a new `Packet` instance.
 */
- (instancetype)initWithString:(NSString *)packetStr;

/**
 Compares the packet with another packet.
 */
- (NSComparisonResult)compare:(Packet *)other;

@end

@interface Packet (Parsing)

/**
 Returns a list of packet elements from its string representation.
 */
+ (NSArray *)parsePacket:(NSString *)packet;

@end

@implementation Packet

- (instancetype)initWithString:(NSString *)packetStr {
    self = [super init];
    if (self != nil) {
        _elements = [Packet parsePacket:packetStr];
    }
    return self;
}

- (NSComparisonResult)compare:(Packet *)other {
    return [Packet compareLeft:self.elements right:other.elements];
}

+ (NSComparisonResult)compareLeft:(NSArray *)left right:(NSArray *)right {
    NSEnumerator *leftEnumerator = [left objectEnumerator];
    NSEnumerator *rightEnumerator = [right objectEnumerator];

    while (YES) {
        NSObject *leftElement = [leftEnumerator nextObject];
        NSObject *rightElement = [rightEnumerator nextObject];

        if (leftElement == nil && rightElement != nil) {
            return NSOrderedAscending;
        }
        if (leftElement != nil && rightElement == nil) {
            return NSOrderedDescending;
        }
        if (leftElement == nil && rightElement == nil) {
            return NSOrderedSame;
        }

        BOOL isLeftList = [leftElement isKindOfClass:[NSArray class]];
        BOOL isRightList = [rightElement isKindOfClass:[NSArray class]];

        if (!isLeftList && !isRightList) {
            int leftValue = [(NSNumber *)leftElement intValue];
            int rightValue = [(NSNumber *)rightElement intValue];

            if (leftValue < rightValue) {
                return NSOrderedAscending;
            }
            if (leftValue > rightValue) {
                return NSOrderedDescending;
            }
            continue;
        }

        NSArray *leftList = isLeftList ? (NSArray *)leftElement : @[ leftElement ];
        NSArray *rightList = isRightList ? (NSArray *)rightElement : @[ rightElement ];

        NSComparisonResult order = [self compareLeft:leftList right:rightList];
        if (order != NSOrderedSame) {
            return order;
        }
    }
}

@end

@implementation Packet (Parsing)

+ (NSArray *)parsePacket:(NSString *)packet {
    NSAssert([packet characterAtIndex:0] == '[', @"Expected packet to be a list.");
    NSInteger nextCharacterIndex = 1;
    return [self parseList:packet nextCharacterIndex:&nextCharacterIndex numberFormatter:[NSNumberFormatter new]];
}

+ (NSArray *)parseList:(NSString *)line 
    nextCharacterIndex:(NSInteger *)nextCharIndexPtr 
       numberFormatter:(NSNumberFormatter *)numberFormatter {

    NSMutableArray *list = [NSMutableArray new];

    unichar nextChar = [line characterAtIndex:*nextCharIndexPtr];
    while (nextChar != ']') {
        if (nextChar == ',') {
            // Skip element separator.
            (*nextCharIndexPtr)++;
        }
        else if (nextChar == '[') {
            // Next element is a list.
            (*nextCharIndexPtr)++;
            NSArray *element = [self parseList:line nextCharacterIndex:nextCharIndexPtr numberFormatter:numberFormatter];
            [list addObject:element];
        }
        else {
            // Next element is a number.
            NSRange currentRange = NSMakeRange(*nextCharIndexPtr, [line length] - *nextCharIndexPtr);

            NSCharacterSet *nonDecimalCharacters = [[NSCharacterSet decimalDigitCharacterSet] invertedSet];
            NSRange endOfNumber = [line rangeOfCharacterFromSet:nonDecimalCharacters options:0 range:currentRange];
            NSAssert(endOfNumber.location != NSNotFound, @"End of number not found.");

            NSString *numberStr = [line substringWithRange:NSMakeRange(*nextCharIndexPtr, endOfNumber.location - *nextCharIndexPtr)];
            NSNumber *element = [numberFormatter numberFromString:numberStr];
            NSAssert(element != nil, @"Unable to parse number.");

            [list addObject:element];
            *nextCharIndexPtr = endOfNumber.location;
        }

        nextChar = [line characterAtIndex:*nextCharIndexPtr];
    }

    // Skip list terminator.
    (*nextCharIndexPtr)++;

    return list;
}

@end


@interface DistressSignal : NSObject

/**
 Reads the input and solves both parts of today's puzzle.
 */
+ (void)solve;

@end

@implementation DistressSignal

+ (void)solve {
    NSArray *packets = [self readPackets];
    [[NSString stringWithFormat:@"Part 1: %ld", [self sumOfInOrderPairIndices:packets]] printLine];
    [[NSString stringWithFormat:@"Part 2: %ld", [self decoderKeyForPackets:packets]] printLine];
}

/**
 Solve part 1.
 */
+ (NSInteger)sumOfInOrderPairIndices:(NSArray *)packets {
    NSInteger sumOfInOrderPairIndices = 0;
    for (NSInteger i = 0; i < [packets count]; i += 2) {
        if ([packets[i] compare:packets[i+1]] == NSOrderedAscending) {
            sumOfInOrderPairIndices += (i / 2) + 1;
        }
    }
    return sumOfInOrderPairIndices;
}

/**
 Solve part 2.
 */
+ (NSInteger)decoderKeyForPackets:(NSArray *)packets {
    NSArray *dividerPackets = @[
        [[Packet alloc] initWithString:@"[[2]]"],
        [[Packet alloc] initWithString:@"[[6]]"],
    ];

    NSArray *packetsWithDividerPackets = [packets arrayByAddingObjectsFromArray:dividerPackets];
    NSArray *sortedPackets = [packetsWithDividerPackets sortedArrayUsingSelector:@selector(compare:)];
    NSUInteger indexOfDividerPacket2 = [sortedPackets indexOfObject:dividerPackets[0]] + 1;
    NSUInteger indexOfDividerPacket6 = [sortedPackets indexOfObject:dividerPackets[1]] + 1;
    return indexOfDividerPacket2 * indexOfDividerPacket6;
}

/**
 Parses the input file into an array of `Packet`.
 */
+ (NSArray *)readPackets {
    NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:NULL];
    NSArray *lines = [fileContents componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];

    NSMutableArray *packets = [NSMutableArray new];
    for (NSString *line in lines) {
        if ([line length] > 0) {
            [packets addObject:[[Packet alloc] initWithString:line]];
        }
    }

    return packets;
}

@end

NS_ASSUME_NONNULL_END

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        [DistressSignal solve];
    }
    return 0;
}
