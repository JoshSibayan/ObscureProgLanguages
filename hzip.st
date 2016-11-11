#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

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

"Check first argument for hyphen to indicate desired flag"
"Then check following arguments for files"
(1 to: Smalltalk arguments size)
    do: [:index |
        argu := (Smalltalk arguments at: index).
        (argu includes: $-) ifTrue: [
            (argu includes: $d) ifTrue: [dFlag := true.].
            (argu includes: $c) ifTrue: [cFlag := true.].
            (argu includes: $t) ifTrue: [tFlag := true.].
            (argu includes: $u) ifTrue: [uFlag := true.].
        ] ifFalse: [
            (infile isEmpty) ifTrue: [
                infile := argu.
            ] ifFalse: [
                outfile :=  argu.
            ]
        ]
    ].

"Print debug information to aid testing"
dFlag ifTrue: [
    'Debug mode' display.
    nl display.

    tFlag ifTrue: [
        'Printing decoding tree' display.
        nl display.
    ]. 

    cFlag ifTrue: [
        'Compressing file ' display.
        infile display.
        (outfile isEmpty) ifFalse: [
            ' into file ' display.
            outfile display.
            nl display.
        ] ifTrue: [
            ' into standard output' display.
            nl display.
        ]
    ].

    uFlag ifTrue: [
        'Decompressing file ' display.
        infile display.
        (outfile isEmpty) ifFalse: [
            ' into file ' display.
            outfile display.
            nl display.
        ] ifTrue: [
            ' to stdout' display.
            nl display.
        ]
    ].    
].

|readfile writefile|
readfile := FileStream open: infile 
                       mode: FileStream read.
writefile := FileStream open: outfile
                        mode: FileStream write.

"Take input from table, store values in sorted collection as tuples"
"Create a queue to sort collection by value"
|freqTable|
uFlag ifFalse: [freqTable := readfile parseFile.].

dFlag ifTrue: [
    (0 to: freqTable size - 1)
    do: [:index |
        index display.
        ' ' display.
        item := freqTable at: index.
        item display.
        nl display.
    ].
].

"Define queue"
|queue|
queue := SortedCollection new.

uFlag ifFalse:[
    (0 to: freqTable size - 1)
    do: [ :index |
        |aLeaf aCount aChar|
        aCount := freqTable at: index.
        aCount isInteger ifTrue: [
            aChar := index asCharacter.
            aLeaf := Leaf new: aChar  count: aCount.
            queue add: aLeaf.
        ].
    ].
].

dFlag ifTrue: [
    queue inspect.
].

"Build tree using queue definition"
uFlag ifFalse: [
    [queue size > 1] whileTrue: [
        |leftChild rightChild aTree treeChar treeCount|
        leftChild := queue removeFirst.
        rightChild := queue removeFirst.
        treeChar := leftChild char.
        treeCount := leftChild count + rightChild count.
        aTree := Tree new: treeChar count: treeCount left: leftChild right: rightChild.
        queue add: aTree.
    ].
].

dFlag ifTrue: [
    queue inspect.
].

|theTree encTable|
uFlag ifFalse: [
    theTree := queue removeLast. 
].

encTable := ZeroArray new: 257.

uFlag ifFalse: [
    theTree inorder: [:char :string |
        |index count|
        index := char asInteger.
        encTable at: index put: string.
    ] prefix: ''.
].

"Run compression algorithm, print decoding tree to stdout"
tFlag ifTrue: [ 
    (0 to: encTable size - 1)
    do: [:index |
        |char range1 range2 count string|
        range1 := index > 32.
        range2 := index < 127.
        count := freqTable at: index.
        string := encTable at: index.

        count isInteger ifTrue: [
            range1 & range2 ifTrue: [
                char := index asCharacter.
                stdout << char << ' ' << count << ' ' << string << nl.
            ] ifFalse: [
                stdout << index << ' ' << count << ' ' << string << nl.
            ].
        ].
    ]. 
    readfile close.
].

"Compress input file, write to output file if specified"
cFlag ifTrue: [ 
    |encString|
    aBitstream := BitStream new: writefile.
    readfile reset.
    theTree binTreeDump: aBitstream.
    aBitstream putBit: 1.
    
    [readfile atEnd not] whileTrue: [
        |index|
        index := readfile next asInteger.
        encString := (encTable at: index).
        aBitstream putString: encString.
    ].

    encString := encTable at: 256.
    aBitstream putString: encString.
    aBitstream padOut.
    readfile close.
    writefile close.
].

"Decompress input file, write to output file if specified"
uFlag ifTrue: [ 
    outBitstream := BitStream new: readfile.
    inBitstream := BitStream new: writefile.
    stack := OrderedCollection new.
    treeWalk := false.
    decTree := nil.

    [(outBitstream atEnd not) & (treeWalk = false)] whileTrue: [
        |nextChar|
        nextChar := outBitstream nextBit.
        nextChar = 0 ifTrue: [
            realChar := 0.
            base2 := 1.

            (1 to: 8) do: [:skip |
                |curBit|
                curBit := outBitstream nextBit.
                (curBit = 1)
                    ifTrue: [realChar := realChar + base2.].
                base2 := base2 *2.
            ].

            realChar = 0 ifTrue: [
                |curBit|
                curBit := outBitstream nextBit.
                curBit = 1 ifTrue: [stack addFirst: (Leaf new: 256 asCharacter count: 1).]
                            ifFalse: [stack addFirst: (Leaf new: 0 asCharacter count: 1).].
            ] ifFalse: [stack addFirst: (Leaf new: realChar asCharacter count: 1).].
      
        ] ifFalse: [
            stack size > 0 ifTrue: [
                right := stack removeFirst.
                stack size = 0 ifTrue: [
                    decTree := right.
                    treeWalk := true.
                ] ifFalse: [
                    left := stack removeFirst.
                    stack addFirst: (Tree new: (left char) count: 1 left: left right: right).
                ].
            ].
        ].
    ].

    dFlag ifTrue: [decTree inspect.].

    tempTree := decTree.
    eof := false.

    [(outBitstream atEnd not) & (eof = false)] whileTrue: [
        |endBit|
        endBit := outBitstream nextBit.

        endBit = 0 ifTrue: [
            tempTree  isLeaf ifTrue: [
                tempTree char asInteger = 256 ifTrue: [eof = true.].
            ] ifFalse: [
                tempTree := tempTree left.
                tempTree  isLeaf ifTrue: [
                    tempTree char asInteger = 256 ifTrue: [ eof = true.]
                        ifFalse: [
                            writefile << tempTree char.
                        ].
                    tempTree := decTree.
                ].
            ].

        ] ifFalse: [
            tempTree isLeaf ifTrue: [
                tempTree char asInteger = 256 ifTrue: [eof := true.].
            ] ifFalse: [
                tempTree = tempTree right.
                tempTree  isLeaf ifTrue: [
                    tempTree char asInteger = 256 ifTrue: [ eof := true.]
                        ifFalse: [
                            writefile << tempTree char.
                        ].
                    tempTree := decTree.
                ].
            ].
        ].
    ].

   readfile close.
   writefile close.
].
