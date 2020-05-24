const insertIntoCommand = require('../commands/insertIntoCommand')
const { InsertIntoSchema } = require('../models/InsertIntoSchema')

describe.each([
    'INSERT INTO Tuotteet (id, nimi, hinta) VALUES (1, \'nauris\', 3);',
    'INSERT INTO Tuotteet (nimi) VALUES (\'kaapo\');',
    'insert into tuotteet (id, nimi, hinta) VALUES (2, \'kurpitsa\', 4);',
    'insERt intO tuoTTeet (id, NIMI, hintA) VALuES (14, \'peruna\', 1);',
    '   insert inTO    tuoTTeet (id       , NIMI, hintA) VALUES (16,     \'tomaatti\', 6);',
    'InSERt        INTO Tuotteet (id, nimi, hinta) VALUES (17, \'sipuli\', 65);',
])('valid command INSERT INTO ... VALUES testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is recognized and true returned', () => {
        const result = insertIntoCommand.isCommand(fullCommandAsStringList)

        expect(result).toBeTruthy()
    })

    test('valid command is parsed and validated successfully', () => {
        const parsedCommand = insertIntoCommand.parseCommand(
            fullCommandAsStringList
        )

        const result = InsertIntoSchema.validate(parsedCommand)

        expect(result.error).not.toBeDefined()
    })
})

describe.each([
    'INSERT INTO Tuotteet id, nimi, hinta) VALUES (1, \'nauris\', 3);', //eka avaava sulku
    'INSERT INTO Tuotteet (id, nimi, hinta) VALUES (3, \'kaapo\', 5)', //puolipiste lopusta
    'insert into tuotteet (id, nimi, hinta) VALUES (2, \'kurpitsa\', 4;', //toka sulkeva sulku
    '       insERt intO       tuoTTeet (id, NIMI, hintA) VALuES;', //arvot puuttuu kokonaan
    '   insert inTO    tuoTTeet (id       , NIMI, hintA) VALUES 16,     \'tomaatti\', 6;', //tokat sulut puuttuu
    'InSERt    Tuotteet (id, nimi, hinta) VALUES (17, \'sipuli\', 65);', //INTO puuttuu
    'INSERT INTO Tuotteet (id, nimi, hinta) VALUES (13, \'kurkku\', 17, 18);', //liikaa arvoja
    'INSERT INTO Tuotteet (id, nimi, hinta) VALUES (1, \'porkkana\');', //liian vähän arvoja
])('invalid command with the right name (CREATE TABLE) testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is parsed but validation fails', () => {
        const parsedCommand = insertIntoCommand.parseCommand(
            fullCommandAsStringList
        )

        const result = InsertIntoSchema.validate(parsedCommand)

        expect(result.error).toBeDefined()
    })
})
/*
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
*/
