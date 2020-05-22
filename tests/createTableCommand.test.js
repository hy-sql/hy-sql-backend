const createTableCommand = require('../commands/createTableCommand')
const { CreateTableSchema } = require('../models/CreateTableSchema')

describe.each([
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY);',
    'create table tuotteet (id integer primary key, nimi text, hinta integer);',
    'create taBle tuoTTeet (id inTEger primary KEY, NIMI text, hintA inTEger);',
    '   create taBle    tuoTTeet (id inTEger    primary KEY, NIMI    text, hintA    inTEger);',
    'crEAte        TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
])('valid command testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is recognized and true returned', () => {
        const result = createTableCommand.isCommand(fullCommandAsStringList)

        expect(result).toBeTruthy()
    })

    test('valid command is parsed and validated successfully', () => {
        const parsedCommand = createTableCommand.parseCommand(
            fullCommandAsStringList
        )

        const result = CreateTableSchema.validate(parsedCommand)

        expect(result.error).toBeFalsy()
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
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is parsed but validation fails', () => {
        const parsedCommand = createTableCommand.parseCommand(
            fullCommandAsStringList
        )

        const result = CreateTableSchema.validate(parsedCommand)

        expect(result.error).toBeTruthy()
    })
})

describe.each([
    'CREATE TABLAE Tuotteet id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
    'creatAe table tuotteet (id integer primary, nimi text, hinta integer);',
    'create taBle! tuoTTeet id inTEger primary KEY, NIMI text, hintA inTEger);',
    '   create taBle_    tuoTTeet (id     primary KEY, NIMI    text, hintA    inTEger);',
])('invalid command name testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is recognized and true returned', () => {
        const result = createTableCommand.isCommand(fullCommandAsStringList)

        expect(result).toBeFalsy()
    })
})
