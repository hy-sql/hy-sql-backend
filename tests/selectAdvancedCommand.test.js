const selectAdvancedCommand = require('../commands/selectAdvancedCommand')
const cleanCommand = require('../utils/cleanCommand')

describe.each([
    'SELECT this+5* FROM Taulu;',
    'SELECT not * going + to -+ work FROM Taulu;',
    'SELECT "not * going + to - work" FROM Taulu;',
    'SELECT this* FROM Taulu;',
    'SELECT this*-that FROM Taulu;',
    'SELECT *this* that FROM Taulu;',
])('SELECT query with invalid arithmetic operation', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = cleanCommand(invalidCommand)

        test('does not contain expression type in fields', () => {
            expect(
                selectAdvancedCommand.parseCommand(command).value
            ).toBeDefined()
            expect(
                selectAdvancedCommand.parseCommand(command).value.fields[0].type
            ).not.toBe('expression')
            expect(
                selectAdvancedCommand.parseCommand(command).value.fields[0].type
            ).toEqual('column')
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
        const command = cleanCommand(validCommand)

        test('contains "fields" field and its type is expression', () => {
            expect(
                selectAdvancedCommand.parseCommand(command).value
            ).toBeDefined()
            expect(
                selectAdvancedCommand.parseCommand(command).value.fields[0].type
            ).toBe('expression')
            expect(
                selectAdvancedCommand.parseCommand(command).error
            ).not.toBeDefined()
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
        const command = cleanCommand(invalidCommand)

        test('contains "fields" field but does not contain field type function and has errors', () => {
            expect(
                selectAdvancedCommand.parseCommand(command).value.fields[0]
            ).toBeDefined()
            expect(
                selectAdvancedCommand.parseCommand(command).value.fields[0].type
            ).not.toBe('function')
            expect(
                selectAdvancedCommand.parseCommand(command).value.fields[0].type
            ).toEqual('column')
            expect(
                selectAdvancedCommand.parseCommand(command).error
            ).toBeDefined()
        })
    })
})

describe.each([
    'SELECT LENGTH(this) FROM Taulu;',
    'SELECT length(caseTest) FROM Taulu;',
    'SELECT lengTH(onceMore) FROM Taulu;',
])('SELECT query with valid function operation', (validCommand) => {
    describe(validCommand, () => {
        const command = cleanCommand(validCommand)

        test('contains "fields" field', () => {
            expect(
                selectAdvancedCommand.parseCommand(command).value
            ).toBeDefined()
            expect(
                selectAdvancedCommand.parseCommand(command).value.fields
            ).toBeDefined()
            expect(
                selectAdvancedCommand.parseCommand(command).error
            ).not.toBeDefined()
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
            const command = cleanCommand(validCommand)

            test('contains "fields" field', () => {
                expect(
                    selectAdvancedCommand.parseCommand(command).value
                ).toBeDefined()
                expect(
                    selectAdvancedCommand.parseCommand(command).value.fields
                ).toBeDefined()
                expect(
                    selectAdvancedCommand.parseCommand(command).error
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
            const command = cleanCommand(validCommand)

            test('contains "fields" field', () => {
                expect(
                    selectAdvancedCommand.parseCommand(command).value
                ).toBeDefined()
                expect(
                    selectAdvancedCommand.parseCommand(command).value.fields
                ).toBeDefined()
                expect(
                    selectAdvancedCommand.parseCommand(command).error
                ).not.toBeDefined()
            })
        })
    }
)
