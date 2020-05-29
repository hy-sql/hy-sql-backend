const selectAllCommand = require('../commands/selectAllCommand')
const commandService = require('../services/commandService')

describe.each([
    'SELEC * FROM Taulu;',
    'SELECT a FROM Taulu BY;',
    'SELECT FROM Taulu ORDER;',
    'SELECT FROM Taulu BY ORDER;',
])('SELECT * query not containing ORDER BY', (wrongCommand) => {
    describe(wrongCommand, () => {
        const command = wrongCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        test('does not contain orderBy field', () => {
            expect(selectAllCommand.parseCommand(command).value).toBeDefined()
            expect(
                selectAllCommand.parseCommand(command).value.orderBy
            ).not.toBeDefined()
        })
    })
})

describe.each([
    'SELECT * FROM Taulu76 order by column ;',
    'SELECT    * FROM        Taulu ORDER BY columnS;',
    'seLEcT * FrOm Taulu   order BY column asc;',
    'SELECT * FROM Taulun_nimi ORder BY column Asc;',
    'SELECT * FROM Tuotteet ORDER BY hinta desc;',
    'SELECT * FROM Tuotteet ORDER BY hinta   dESc  ;',
])('Valid SELECT * ORDER BY -query', (validCommand) => {
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
            expect(parsedCommand.value).toHaveProperty('orderBy')

            if (selectAllCommand.parseCommand(command).error) {
                console.log('ERRORR HERE')
                console.log(parsedCommand.value.orderBy.order)
            }

            expect(selectAllCommand.parseCommand(command).error).toBeUndefined()
        })
    })
})

describe.each([
    'SELECT * FROM Taulu order by where where;',
    'SELECT * FROM Taulu order by asc hi;',
    'SELECT * FROM Taulu order by asc hi:',
    'SELECT * FROM Taulu order asc by;',
    'SELECT * FROM Taulu by order',
    'SELECT * FROM Taulu order by)a;',
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
