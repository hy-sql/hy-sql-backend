const selectCommand = require('../commands/selectCommand')
const { SelectSchema } = require('../models/SelectSchema')
const commandService = require('../services/commandService')

describe.each([
    'SELECT id, nimi, hinta FROM Tuotteet;',
    'SELECT nimi FROM Tuotteet;',
    'select id, nimi, hinta from tuotteet;',
    'seLEct  id, NIMI, hintA FROM tuoTTeet;',
    '   selecT     id       , NIMI, hintA FRoM tuoTTeet;',
    'select id, NIMI, hinta fROM                    Tuotteet;',
])('valid command SELECT ... FROM testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is recognized and true returned', () => {
        const result = commandService.parseCommand(fullCommandAsStringList)

        expect(result).toBeTruthy()
    })

    test('valid command is parsed and validated successfully', () => {
        const parsedCommand = selectCommand.parseCommand(
            fullCommandAsStringList
        )

        expect(parsedCommand.error).not.toBeDefined()
    })
})

describe.each([
    'SELECT id nimi, hinta FROM Tuotteet;', //eka sarakkeiden pilkku puuttuu
    'SELECT id,nimi,hinta FROM Tuotteet', //puolipiste lopusta
    'SELECT id,nimi,hinta FROM ;', //taulu puuttuu
    '       seLEct FROM      tuoTTeet ;', //sarakkeet puuttuu kokonaan
    '   selecT id nimi hinta FROM tuoTTeeT;', //sarakkeiden kaikki pilkut puuttuu
    'SeleCT id,nimi,hinta   Tuotteet;', //FROM puuttuu
])('invalid command with the right name (SELECT) testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is parsed but validation fails', () => {
        const parsedCommand = selectCommand.parseCommand(
            fullCommandAsStringList
        )

        const result = SelectSchema.validate(parsedCommand)

        expect(result.error).toBeDefined()
    })
})
//SELECT id,nimi,hinta FROM Tuotteet;
describe.each([
    'SELECT* id,nimi,hinta FROM Tuotteet;',
    'SELECTid,nimi,hinta FROM Tuotteet;',
    'seleeect id,nimi,hinta from TUOTTEET;',
    'ELECT id,nimi,hinta FroM Tuotteet;',
])('invalid command name(SELECT) testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('invalid command is NOT recognized and false returned', () => {
        const result = commandService.parseCommand(fullCommandAsStringList)

        expect(result).toBeFalsy()
    })
})
