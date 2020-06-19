const createTableParser = require('../../commandParsers/createTableParser')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY);',
    'create table tuotteet (id integer primary key, nimi text, hinta integer);',
    'create taBle tuoTTeet (id inTEger primary KEY, NIMI text, hintA inTEger);',
    '   create taBle    tuoTTeet (id inTEger    primary KEY, NIMI    text, hintA    inTEger);',
    'crEAte        TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
])('valid command testing', (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)

    test('valid command is recognized and true returned', () => {
        const result = commandService.parseCommand(fullCommandAsStringArray)

        expect(result).toBeTruthy()
    })

    test('valid command is parsed and validated successfully', () => {
        const parsedCommand = createTableParser.parseCommand(
            fullCommandAsStringArray
        )

        expect(parsedCommand).toBeDefined()
        expect(parsedCommand).toHaveProperty('name')
        expect(parsedCommand).toHaveProperty('openingBracket')
        expect(parsedCommand).toHaveProperty('columns')
        expect(parsedCommand).toHaveProperty('closingBracket')
        expect(parsedCommand).toHaveProperty('finalSemicolon')
    })
})

describe.each([
    'CREATE TABLE Tuotteet id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
    'create table tuotteet (id integer primary, nimi text, hinta integer)',
    'create taBle tuoTTeet id inTEger primary KEY, NIMI text, hintA inTEger);',
    '   create taBle    tuoTTeet (id     primary KEY, NIMI    text, hintA    inTEger);',
    '   create taBle    tuoTTeet ',
    '   create      ',
])('invalid command with the right name (CREATE TABLE) testing', (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)

    test('fails validation after parsed to command object', () => {
        expect(() => {
            commandService.parseCommand(fullCommandAsStringArray)
        }).toThrow()
    })
})

describe.each([
    'CREATE TABLAE Tuotteet id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
    'creatAe table tuotteet (id integer primary, nimi text, hinta integer);',
    'create taBle! tuoTTeet id inTEger primary KEY, NIMI text, hintA inTEger);',
    '   create taBle_    tuoTTeet (id     primary KEY, NIMI    text, hintA    inTEger);',
])('invalid command name testing', (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)

    test('fails validation after parsed to command object', () => {
        expect(() => {
            commandService.parseCommand(fullCommandAsStringArray)
        }).toThrow()
    })
})
