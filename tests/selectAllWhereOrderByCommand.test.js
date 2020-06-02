const commandService = require('../services/commandService')

describe.each([
    'SELECT * FROM Taulu ;',
    'SELECT * FROM Taulu  ORDER hinta;',
    'SELECT * FROM Taulu ORDER BY that;',
    'SELECT * FROM Taulu ORDER BY that;',
])('SELECT * query not containing WHERE keyword', (wrongCommand) => {
    describe(wrongCommand, () => {
        const command = wrongCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        test('does not contain where field', () => {
            const parsedCommand = commandService.parseCommand(command)
            expect(parsedCommand.value).not.toHaveProperty('where')
        })
    })
})

describe.each([
    'SELECT * FROM Taulu WHERE this="that";',
    'SELECT * FROM Taulu WHERE this=4;',
    'SELECT * FROM Taulu WHERE BY ORDER;',
    'SELECT * FROM WHERE Taulu BY that ORDER;',
])(
    'SELECT * query not containing ORDER BY keywords or format invalid',
    (wrongCommand) => {
        describe(wrongCommand, () => {
            const command = wrongCommand
                .trim()
                .replace(/\s\s+/g, ' ')
                .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

            test('does not contain orderBy field or format invalid', () => {
                const parsedCommand = commandService.parseCommand(command)
                expect(parsedCommand.value).not.toHaveProperty('orderBy')
            })
        })
    }
)

describe.each([
    'SELECT * FROM Taulu where order by;',
    'SELECT * FROM Taulu where this order by this;',
    'SELECT * FROM Taulu where this-that order by that:',
    'SELECT * FROM Taulu where #=5 order by 5;',
    'SELECT * FROM Taulu where is=true order by false',
    'SELECT * FROM Taulu where "this"="that" order by that;',
    "SELECT * FROM Taulu76 where name='test' ' order by column;",
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
            const parsedCommand = commandService.parseCommand(command)
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.value).toHaveProperty('orderBy')
            expect(parsedCommand.error).toBeDefined()
        })
    })
})

describe.each([
    "SELECT * FROM Taulu76 where name='test' order by column;",
    'SELECT    * FROM        Taulu  WheRe hinta<4 ORDER BY hinta;',
    'seLEcT * FrOm Taulu  whERE hinta>6 order BY hinta asc;',
    'SELECT * FROM Taulun_nimi where taulu=9 ORder BY column Asc;',
    'seLEcT * FrOm Taulu  whERE hinta>6 order BY hinta desc;',
    "SELECT * FROM Tuotteet where column='table' ORDER BY table   dESc  ;",
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
            const parsedCommand = commandService.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.value).toHaveProperty('orderBy')

            expect(commandService.parseCommand(command).error).toBeUndefined()
        })
    })
})
