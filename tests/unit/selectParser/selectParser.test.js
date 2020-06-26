const selectParser = require('../../../commandParsers/selectParser')
const commandService = require('../../../services/commandService')
const splitCommandIntoArray = require('../../../commandParsers/parserTools/splitCommandIntoArray')
const SQLError = require('../../../models/SQLError')

describe.each([
    'SELECT id,nimi,hinta FROM Tuotteet;',
    'SELECT id, nimi, hinta FROM Tuotteet;',
    'SELECT nimi FROM Tuotteet;',
    'select id, nimi, hinta from tuotteet;',
    'seLEct  id, NIMI, hintA FROM tuoTTeet;',
    '   selecT     id       , NIMI, hintA FRoM tuoTTeet;',
    'select id, NIMI, hinta fROM                    Tuotteet;',
])('valid command SELECT ... FROM testing', (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)

    test('valid command is recognized and true returned', () => {
        const result = commandService.parseCommand(fullCommandAsStringArray)

        expect(result).toBeTruthy()
    })

    test('valid command is parsed and validated successfully', () => {
        const parsedCommand = selectParser.parseCommand(
            fullCommandAsStringArray
        )

        expect(parsedCommand).toBeDefined()
    })
})

describe.each([
    'SELECT id nimi, hinta FROM Tuotteet;',
    'SELECT id,nimi,hinta FROM Tuotteet',
    'SELECT id,nimi,hinta FROM ;',
    '       seLEct FROM      tuoTTeet ;',
    '   selecT id nimi hinta FROM tuoTTeeT;',
    'SeleCT id,nimi,hinta   Tuotteet;',
])('invalid command with the right name (SELECT) testing', (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)

    test('valid command is parsed but validation fails', () => {
        expect(() =>
            selectParser.parseCommand(fullCommandAsStringArray)
        ).toThrowError()
    })
})

describe.each([
    'SELECT* id,nimi,hinta FROM Tuotteet;',
    'SELECTid,nimi,hinta FROM Tuotteet;',
    'seleeect id,nimi,hinta from TUOTTEET;',
    'ELECT id,nimi,hinta FroM Tuotteet;',
])('invalid command name(SELECT) testing', (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)

    test('invalid command is NOT recognized and false returned', () => {
        expect(() =>
            commandService.parseCommand(fullCommandAsStringArray)
        ).toThrowError()
    })
})

describe.each([
    'SELECT this+5* FROM Taulu;',
    'SELECT not * going + to -+ work FROM Taulu;',
    'SELECT "not * going + to - work" FROM Taulu;',
    'SELECT this* FROM Taulu;',
    'SELECT this*-that FROM Taulu;',
    'SELECT *this* that FROM Taulu;',
])('SELECT query with invalid arithmetic operation', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('does not contain expression type in fields', () => {
            expect(() => selectParser.parseCommand(command)).toThrowError()
        })
    })
})

describe.each([
    'SELECT this+5 FROM Taulu;',
    'SELECT is*going+to-work FROM Taulu;',
    'SELECT this+that*5 FROM Taulu;',
    'SELECT CASE+test FROM Taulu;',
    'SELECT once*MORE+tHat FROM Taulu;',
])('SELECT query with valid arithmetic operation', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('contains "fields" field and its type is expression', () => {
            expect(selectParser.parseCommand(command)).toBeDefined()
            expect(selectParser.parseCommand(command).fields[0].type).toBe(
                'expression'
            )
        })
    })
})

describe.each([
    'SELECT LENGTH.(this) FROM Taulu;',
    'SELECT *length(caseTest) FROM Taulu;',
    'SELECT lengTH((onceMore) FROM Taulu;',
    'SELECT lengTH(onceMore)) FROM Taulu;',
])('SELECT query with invalid function operation', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('contains "fields" field but does not contain field type function and has errors', () => {
            expect(() => selectParser.parseCommand(command)).toThrowError()
        })
    })
})

describe.each([
    'SELECT LENGTH(this) FROM Taulu;',
    'SELECT length(caseTest) FROM Taulu;',
    'SELECT lengTH(onceMore) FROM Taulu;',
])('SELECT query with valid function operation', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('contains "fields" field', () => {
            expect(selectParser.parseCommand(command)).toBeDefined()
            expect(selectParser.parseCommand(command).fields).toBeDefined()
            expect(selectParser.parseCommand(command).error).not.toBeDefined()
        })
    })
})

describe.each([
    'SELECT this+5, LENGTH(this) FROM Taulu;',
    'SELECT is*going+to-work, length(caseTest) FROM Taulu;',
    'SELECT lengTH(onceMoreBackwards), this+that*5 FROM Taulu;',
    'SELECT CASE+test, length(test) FROM Taulu;',
    'SELECT length(test), once*MORE+tHat FROM Taulu;',
])(
    'SELECT query with valid arithmetic and function operation',
    (validCommand) => {
        describe(validCommand, () => {
            const command = splitCommandIntoArray(validCommand)

            test('contains "fields" field', () => {
                expect(selectParser.parseCommand(command)).toBeDefined()
                expect(selectParser.parseCommand(command).fields).toBeDefined()
                expect(
                    selectParser.parseCommand(command).error
                ).not.toBeDefined()
            })
        })
    }
)

describe.each([
    'SELECT this+5, LENGTH(this), hinta FROM Taulu;',
    'SELECT is*going+to-work, hinta, length(caseTest) FROM Taulu;',
    'SELECT hinta, lengTH(onceMoreBackwards), this+that*5 FROM Taulu;',
    'SELECT testi, CASE+test, testiKaksi, length(test) FROM Taulu;',
    'SELECT hello, length(test), does, once*MORE+tHat, it, work FROM Taulu;',
])(
    'SELECT query with valid arithmetic and function operation and standard column',
    (validCommand) => {
        describe(validCommand, () => {
            const command = splitCommandIntoArray(validCommand)

            test('contains "fields" field', () => {
                expect(selectParser.parseCommand(command)).toBeDefined()
                expect(selectParser.parseCommand(command).fields).toBeDefined()
                expect(
                    selectParser.parseCommand(command).error
                ).not.toBeDefined()
            })
        })
    }
)

describe.each([
    'SELECT DISTINCT nimi, hinta FROM Tuotteet;',
    'SELECT DISTINCT nimi FROM Tuotteet;',
    'select distinct nimi from Tuotteet;',
    'select dIstINcT nimi from Tuotteet;',
])('SELECT query with DISTINCT keyword', (validCommand) => {
    describe(`parsed command ${validCommand}`, () => {
        const command = splitCommandIntoArray(validCommand)

        test('contains "fields" field', () => {
            expect(selectParser.parseCommand(command)).toBeDefined()
            expect(selectParser.parseCommand(command).fields).toBeDefined()
        })

        test('"fields" contains correct type', () => {
            const parsed = selectParser.parseCommand(command)
            expect(parsed.fields[0].type).toBe('distinct')
            expect(parsed.fields[0]).toBeDefined()
        })
    })
})

describe.each(['SELECT DISTIN nimi, hinta FROM Tuotteet;'])(
    'INVALID SELECT query with DISTINCT keyword',
    (invalidCommand) => {
        describe(`Invalid command ${invalidCommand}`, () => {
            const command = splitCommandIntoArray(invalidCommand)

            test('throws error because fields are not separated by comma', () => {
                expect(() => selectParser.parseCommand(command)).toThrowError(
                    new SQLError('fields must be split by comma (,)')
                )
            })
        })
    }
)

describe('Invalid SELECT query containing OFFSET without LIMIT', () => {
    test('causes correct error to be thrown during parsing', () => {
        const command = splitCommandIntoArray(
            'SELECT nimi, hinta FROM Tuotteet OFFSET 2;'
        )

        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError(
                'Query contains OFFSET keyword without containing LIMIT keyword.'
            )
        )
    })
})

describe.each([
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 WHERE hinta=2;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 ORDER BY hinta;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 GROUP BY hinta;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 HAVING hinta;',
])(
    'Invalid query containing LIMIT incorrectly placed before other keywords',
    (invalidCommand) => {
        describe(invalidCommand, () => {
            const command = splitCommandIntoArray(invalidCommand)

            test('throws correct error during parsing', () => {
                expect(() => selectParser.parseCommand(command)).toThrowError(
                    new SQLError(
                        'LIMIT must always be positioned after WHERE, GROUP BY, HAVING and ORDER BY'
                    )
                )
            })
        })
    }
)

describe('Invalid query with OFFSET incorrectly placed before LIMIT', () => {
    const command = splitCommandIntoArray(
        'SELECT nimi, hinta FROM Tuotteet OFFSET 2 LIMIT 2;'
    )

    test('throws correct error during parsing', () => {
        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError('OFFSET must always be positioned after LIMIT')
        )
    })
})

describe('Invalid SELECT-query containing incorrectly placed keywords', () => {
    const queries = [
        'SELECT nimi, hinta FROM Tuotteet GROUP BY nimi WHERE hinta>2;',
        'SELECT nimi, hinta FROM Tuotteet HAVING COUNT(*)>1 WHERE hinta>2;',
        'SELECT nimi, hinta FROM Tuotteet ORDER BY nimi WHERE hinta>2;',
        'SELECT nimi, hinta FROM Tuotteet HAVING COUNT(*)>1 GROUP BY hinta;',
        'SELECT nimi, hinta FROM Tuotteet ORDER BY nimi GROUP BY hinta;',
        'SELECT nimi, hinta FROM Tuotteet ORDER BY HAVING COUNT(*)>1 hinta;',
    ]

    test(`'${queries[0]}' throws correct error during parsing`, () => {
        const command = splitCommandIntoArray(queries[0])

        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError(
                'WHERE must always be positioned before GROUP BY, HAVING and ORDER BY'
            )
        )
    })

    test(`'${queries[1]}' throws correct error during parsing`, () => {
        const command = splitCommandIntoArray(queries[1])

        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError(
                'WHERE must always be positioned before GROUP BY, HAVING and ORDER BY'
            )
        )
    })

    test(`'${queries[2]}' throws correct error during parsing`, () => {
        const command = splitCommandIntoArray(queries[2])

        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError(
                'WHERE must always be positioned before GROUP BY, HAVING and ORDER BY'
            )
        )
    })

    test(`'${queries[3]}' throws correct error during parsing`, () => {
        const command = splitCommandIntoArray(queries[3])

        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError(
                'GROUP BY must always be positioned after WHERE and before HAVING and ORDER BY'
            )
        )
    })

    test(`'${queries[4]}' throws correct error during parsing`, () => {
        const command = splitCommandIntoArray(queries[4])

        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError(
                'GROUP BY must always be positioned after WHERE and before HAVING and ORDER BY'
            )
        )
    })

    test(`'${queries[5]}' throws correct error during parsing`, () => {
        const command = splitCommandIntoArray(queries[5])

        expect(() => selectParser.parseCommand(command)).toThrowError(
            new SQLError(
                'HAVING must always be positioned after WHERE and GROUP BY and before ORDER BY'
            )
        )
    })
})
