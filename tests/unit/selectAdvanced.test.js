const selectParser = require('../../commandParsers/selectParser')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

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
            expect(selectParser.parseCommand(command).error).not.toBeDefined()
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

            test('contains "fields" but field type is not "distinct"', () => {
                const parsed = selectParser.parseCommand(command)
                expect(parsed.fields).toBeDefined()
                expect(parsed.fields[0].type).not.toBe('distinct')
                // expect(selectParser.parseCommand(command).error).toBeDefined()
            })
        })
    }
)
