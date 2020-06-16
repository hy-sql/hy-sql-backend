const selectParser = require('../../commandParsers/selectParser')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    'SELECT nimi, hinta FROM Taulu ;',
    'SELECT nimi, hinta FROM Taulu  ORDER hinta;',
    'SELECT nimi, hinta FROM Taulu ORDER BY that;',
    'SELECT nimi, hinta FROM Taulu ORDER BY that;',
])('SELECT nimi, hinta query not containing WHERE keyword', (wrongCommand) => {
    describe(wrongCommand, () => {
        const command = splitCommandIntoArray(wrongCommand)

        test('does not contain where field', () => {
            const parsedCommand = selectParser.parseCommand(command)
            expect(parsedCommand.value).not.toHaveProperty('where')
        })
    })
})

describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE this="that";',
    'SELECT nimi, hinta FROM Taulu WHERE this=4;',
    'SELECT nimi, hinta FROM Taulu WHERE BY ORDER;',
    'SELECT nimi, hinta FROM WHERE Taulu BY that ORDER;',
])(
    'SELECT nimi, hinta query not containing ORDER BY keywords or format invalid',
    (wrongCommand) => {
        describe(wrongCommand, () => {
            const command = splitCommandIntoArray(wrongCommand)

            test('does not contain orderBy field or format invalid', () => {
                const parsedCommand = selectParser.parseCommand(command)
                expect(parsedCommand.value).not.toHaveProperty('orderBy')
            })
        })
    }
)

describe.each([
    'SELECT nimi, hinta FROM Taulu where order by;',
    'SELECT nimi, hinta FROM Taulu where this order by this;',
    'SELECT nimi, hinta FROM Taulu where this-that order by that:',
    'SELECT nimi, hinta FROM Taulu where #=5 order by 5;',
    'SELECT nimi, hinta FROM Taulu where is=true order by false',
    'SELECT nimi, hinta FROM Taulu where "this"="that" order by that;',
    // "SELECT nimi, hinta FROM Taulu76 where name='test' ' order by column;", FIXME: Additional after where part
])('Invalid SELECT nimi, hinta -query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('is recognised as a command', () => {
            expect(selectParser.parseCommand(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            const parsedCommand = selectParser.parseCommand(command)
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.value).toHaveProperty('orderBy')
            expect(parsedCommand.error).toBeDefined()
        })
    })
})

describe.each([
    "SELECT nimi, hinta FROM Taulu76 where name='test' order by column;",
    'SELECT    nimi, hinta FROM        Taulu  WheRe hinta<4 ORDER BY hinta;',
    'seLEcT nimi, hinta FrOm Taulu  whERE hinta>6 order BY hinta asc;',
    'SELECT nimi, hinta FROM Taulun_nimi where taulu=9 ORder BY column Asc;',
    'seLEcT nimi, hinta FrOm Taulu  whERE hinta>6 order BY hinta desc;',
    "SELECT nimi, hinta FROM Tuotteet where column='table' ORDER BY table   dESc  ;",
])('Valid SELECT nimi, hinta ORDER BY -query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised as a command', () => {
            expect(selectParser.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = selectParser.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.value).toHaveProperty('orderBy')

            expect(selectParser.parseCommand(command).error).toBeUndefined()
        })
    })
})
