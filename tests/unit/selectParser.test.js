const selectParser = require('../../commandParsers/selectParser')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

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
    // 'SELECT id nimi, hinta FROM Tuotteet;', //eka sarakkeiden pilkku puuttuu //FIXME: Select parser bug #107
    'SELECT id,nimi,hinta FROM Tuotteet', //puolipiste lopusta
    'SELECT id,nimi,hinta FROM ;', //taulu puuttuu
    '       seLEct FROM      tuoTTeet ;', //sarakkeet puuttuu kokonaan
    // '   selecT id nimi hinta FROM tuoTTeeT;', //sarakkeiden kaikki pilkut puuttuu //FIXME: Select parser bug #107
    'SeleCT id,nimi,hinta   Tuotteet;', //FROM puuttuu
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
