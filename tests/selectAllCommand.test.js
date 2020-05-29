const selectAllCommand = require('../commands/selectAllCommand')
const commandService = require('../services/commandService')

describe.each(['SELEC * FROM Taulu;', 'SELECT FROM Taulu;'])(
    'Query not beginning with SELECT *',
    (wrongCommand) => {
        describe(wrongCommand, () => {
            const command = wrongCommand
                .trim()
                .replace(/\s\s+/g, ' ')
                .replace(/\s+,/g, ',')
                .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
                .filter(Boolean)

            test('does not pass validation', () => {
                expect(
                    selectAllCommand.parseCommand(command).error
                ).toBeDefined()
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
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

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

            expect(parsedCommand.error).toBeUndefined()
        })
    })
})

describe.each([
    'SELECT * FRM Taulu;',
    'SELECT * FROM Tau&lu;',
    'SELECT * FROM Taulu:',
    'SELECT * FROM Taulu',
    'SELECT * FROM',
    'SELECT * FROM Taulu)a;',
    'SELECT * FRM Taulu;',
    'SELECT * FRM Taulu additonal ;',
])('Invalid SELECT * -query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = invalidCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            expect(selectAllCommand.parseCommand(command).error).toBeDefined()
        })
    })
})

describe.each([
    'SELECT * FROM Tuotteet WHERE price=7;',
    'SELECT * FROM Tuotteet WhEre price=7;',
    'SELECT * FROM Tuotteet WHERE price = 7;',
    'SELECT * FROM Tuotteet WHERE price =7;',
    "SELECT * FROM Tuotteet WHERE name = ' test ';",
    "SELECT * FROM Tuotteet WHERE name=' test';",
    "SELECT * FROM Tuotteet WHERE name='test ';",
    "SELECT * FROM Tuotteet WHERE name='test';",
])('Valid SELECT * FROM ... WHERE ...-query', (validCommand) => {
    describe(validCommand, () => {
        const command = validCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = selectAllCommand.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.error).toBeUndefined()
        })
    })
})

describe.each([
    'SELECT * FROM Tuotteet WHERE  = 7;',
    'SELECT * FROM Tuotteet WHERE price 7;',
    'SELECT * FROM Tuotteet WHERE price7;',
    "SELECT * FROM Tuotteet WHERE name = ' test '",
    "SELECT * FROM Tuotteet WHERE name='';",
    "SELECT * FROM Tuotteet WHERE name name='test';",
    "SELECT * FROM Tuotteet WHERE name='test' additional;",
])('Invalid SELECT * FROM ... WHERE ...-query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = invalidCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            const parsedCommand = selectAllCommand.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.error).toBeDefined()
        })
    })
})

describe.each([
    'SELECT * FROM Tuotteet WHEE price=7;',
    'SELECT * FROM Tuotteet  price=7;',
])(
    'SELECT * FROM ... WHERE ...-query with misspelled or missing WHERE',
    (validCommand) => {
        describe(validCommand, () => {
            const command = validCommand
                .trim()
                .replace(/\s\s+/g, ' ')
                .replace(/\s+,/g, ',')
                .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
                .filter(Boolean)

            test('is recognised as a command', () => {
                expect(commandService.parseCommand(command)).toBeTruthy()
            })

            test('fails validation after parsed to command object', () => {
                const parsedCommand = selectAllCommand.parseCommand(command)

                expect(parsedCommand.value).toBeDefined()
                expect(parsedCommand.value).not.toHaveProperty('where')
                expect(parsedCommand.error).toBeDefined()
            })
        })
    }
)
