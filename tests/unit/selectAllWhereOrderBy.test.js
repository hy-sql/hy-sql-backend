const { parseCommand } = require('../../commandParsers/selectParser')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    'SELECT * FROM Taulu ;',
    'SELECT * FROM Taulu  ORDER hinta;',
    'SELECT * FROM Taulu ORDER BY that;',
    'SELECT * FROM Taulu ORDER BY that;',
])('SELECT * query not containing WHERE keyword', (wrongCommand) => {
    describe(wrongCommand, () => {
        const command = splitCommandIntoArray(wrongCommand)

        test('does not contain where field', () => {
            const parsedCommand = parseCommand(command)
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
            const command = splitCommandIntoArray(wrongCommand)

            test('does not contain orderBy field or format invalid', () => {
                const parsedCommand = parseCommand(command)
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
    // "SELECT * FROM Taulu76 where name='test' ' order by column;", FIXME: Additional after where part
])('Invalid SELECT * -query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        const parsedCommand = parseCommand(command)

        test('is recognised as a command', () => {
            expect(parsedCommand).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            const parsedCommand = parseCommand(command)
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
        const command = splitCommandIntoArray(validCommand)

        test('is recognised as a command', () => {
            expect(parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.value).toHaveProperty('orderBy')

            expect(parseCommand(command).error).toBeUndefined()
        })
    })
})
