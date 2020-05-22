const selectAllCommand = require('../commands/selectAllCommand')
const selectAllSchema = require('../models/SelectAllSchema')

describe.each(['SELEC * FROM Taulu;', 'SELECT a FROM Taulu;'])(
    'Query not beginning with SELECT *',
    (wrongCommand) => {
        describe(wrongCommand, () => {
            const command = wrongCommand
                .trim()
                .replace(/\s\s+/g, ' ')
                .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

            test('is not reconised as SELECT * -command', () => {
                expect(selectAllCommand.isCommand(command)).toBeFalsy()
            })
        })
    }
)

describe.each([
    'SELECT * FROM Taulu76;',
    'SELECT    * FROM        Taulu;',
    'seLEcT * FrOm Taulu;',
    'SELECT * FROM Taulun_nimi;',
])('Valid SELECT * -query', (validCommand) => {
    describe(validCommand, () => {
        const command = validCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        const parsedCommand = selectAllCommand.parseCommand(command)

        test('is recognised as SELECT * -command', () => {
            expect(selectAllCommand.isCommand(command)).toBeTruthy()
        })

        test('passes validation after parsed to command object', () => {
            expect(
                selectAllSchema.validate(parsedCommand).error
            ).toBeUndefined()
        })

        test('execute returns command object successfully', () => {
            expect(selectAllCommand.execute(command).value).toHaveProperty(
                'name'
            )
            expect(selectAllCommand.execute(command).value).toHaveProperty(
                'from'
            )
            expect(selectAllCommand.execute(command).value).toHaveProperty(
                'tableName'
            )
            expect(selectAllCommand.execute(command).value).toHaveProperty(
                'finalSemicolon'
            )
        })
    })
})

describe.each([
    'SELECT * FRM Taulu;',
    'SELECT * FROM Tau&lu;',
    'SELECT * FROM Taulu:',
    'SELECT * FROM Taulu',
    'SELECT * FROM',
])('Invalid SELECT * -query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = invalidCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        const parsedCommand = selectAllCommand.parseCommand(command)

        test('is recognised as SELECT * -command', () => {
            expect(selectAllCommand.isCommand(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            expect(selectAllSchema.validate(parsedCommand).error).toBeDefined()
        })

        test('execute does not return command object', () => {
            expect(selectAllCommand.execute(command)).not.toHaveProperty(
                'tableName'
            )
            expect(selectAllCommand.execute(command)).not.toHaveProperty(
                'finalSemicolon'
            )
        })
    })
})
