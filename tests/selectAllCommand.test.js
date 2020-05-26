const selectAllCommand = require('../commands/selectAllCommand')
const commandService = require('../services/CommandService')

describe.each([
    'SELEC * FROM Taulu;',
    'SELECT a FROM Taulu;',
    'SELECT FROM Taulu;',
])('Query not beginning with SELECT *', (wrongCommand) => {
    describe(wrongCommand, () => {
        const command = wrongCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        test('does not pass validation', () => {
            expect(selectAllCommand.parseCommand(command).error).toBeDefined()
        })
    })
})

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

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = selectAllCommand.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('name')
            expect(parsedCommand.value).toHaveProperty('from')
            expect(parsedCommand.value).toHaveProperty('tableName')
            expect(parsedCommand.value).toHaveProperty('finalSemicolon')

            expect(selectAllCommand.parseCommand(command).error).toBeUndefined()
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

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            expect(selectAllCommand.parseCommand(command).error).toBeDefined()
        })
    })
})
