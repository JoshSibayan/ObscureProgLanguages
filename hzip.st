#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"Taken from charfreq.st example"
"Used to alleviate file parsing issues since smalltalk is non-zero indexed"
Object subclass: ZeroArray [
    |theArray|

    ZeroArray class >> new [
        self shouldNotImplement.
    ]

    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    init: size [
        theArray := Array new: size.
    ]

    size [
        ^ theArray size.
    ]

    at: index [
        ^ theArray at: index + 1.
    ]

    at: index put: value [
        ^ theArray at: index + 1 put: value.
    ]

    incr: index [
        (self at: index) isNil
            ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]

"Taken from catfile.st example"
"Added file parsing functionality"
FileStream extend [
    nl := Character nl.
    catFile [
        |count|
        count := 0.
        [self atEnd not] whileTrue: [
            count := count +1.
            count asString size to: 5 do: [:skip| stdout << ' '].
            stdout << count << ': ' << self nextLine << nl.
        ]
    ]

    parseFile [
        |counttable|
        counttable := ZeroArray new: 257.
        [self atEnd not] whileTrue: [
            |nextChar|
            nextChar := self next asInteger.
            counttable incr: nextChar.
        ].
        counttable incr: 256.
        ^counttable
    ]
]

"Taken from bitstream.st example"
"Added putBit, putString, padOut functions"
Object subclass: BitStream [
    |stringIndex bitIndex byte myStream writeIndex writeByte|

    BitStream class >> new: fileStream [
        |result|
        result := super new.
        result init: fileStream.
        ^ result
    ]

    init: fileStream [
        myStream := fileStream.
        bitIndex := 1.
        writeIndex := 9.
        writeByte := 0.
    ]

    nextBit [
        bitIndex = 1 ifTrue: [
            byte := myStream next.
            bitIndex := 9.
        ].
        bitIndex := bitIndex - 1.
        ^ byte value bitAt: bitIndex
    ]

    putBit: theBit [
        writeIndex := writeIndex - 1.
        writeByte := writeByte bitAt: writeIndex put: theBit.
        writeIndex = 1 ifTrue: [
            myStream nextPutByte: writeByte.
            writeByte := 0.
            writeIndex := 9.
        ].
    ]

    putString: theString [
        theString do: [:index|
            (index asString = '0')
                ifTrue: [self putBit: 0]
                ifFalse: [self putBit: 1].
        ].
    ]

    padOut [
        [writeIndex > 1 & writeIndex < 9] whileFalse: [ 
            self putBit: 0. 
            writeIndex := writeIndex - 1.].
    ]

    atEnd [
        ^ bitIndex = 1 and: [myStream atEnd]
    ]
]

"Taken from tree.st example"
"Definition of Leaf class extended with tree dump function"
Object subclass: Leaf [
    |char count|
    char [ ^ char ]
    count [ ^ count ]
    isLeaf [ ^ true ]

    Leaf class >> new [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^ result
    ]

    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
    ]

    <= other [
        ^ (count < other count) | ((count = other count) & (char <= other char))
    ]

    printBase: aStream [
        ^ aStream << self class << '(' << char << ',' << count
    ]

    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]

    binTreeDump: aBitstream [
        aBitstream putBit: 0.
        (1 to: 8) do: [:index | 
            aBitstream putBit: ( (self char asInteger) bitAt: index) ].
        char asInteger = 0 ifTrue: [
            aBitstream putBit: 0.
        ].
        char asInteger = 256 ifTrue: [
            aBitstream putBit: 1.
        ]
    ]
]

"Also taken from tree.st example"
"Added functionality to step down tree and print nodes"
Leaf subclass: Tree [
    |left right|

    Tree class >> new: aChar count: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]

    isLeaf [ 
        ^ false 
    ]

    setLeft: aLeft andRight: aRight [
        left := aLeft.
        right := aRight.
    ]

    right [ 
        ^ right 
    ]
    
    left [ 
        ^ left 
    ]

    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]

    inorder: visitor prefix: string [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]

    binTreeDump: aBitstream [
        left binTreeDump: aBitstream.
        right binTreeDump: aBitstream.
        aBitstream putBit: 1.
    ]
]

nl := Character nl.
dFlag := false.
cFlag := false.
tFlag := false.
uFlag := false.
infile := ''.
outfile := stdout.






