/*eslint-disable no-unused-vars*/
const selectAdvancedCommand = require('../commands/selectAdvancedCommand')
const cleanCommand = require('../utils/cleanCommand')

describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE hinta+1=5;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*2=20;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*hinta>=20;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*h<=20;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta/2*hinta<=20;',
])(
    'SELECT with WHERE condition containing arithmetic expression',
    (validCommand) => {
        const command = cleanCommand(validCommand)

        describe(validCommand, () => {
            test('is valid and parsed', () => {
                const parsedCommand = selectAdvancedCommand.parseCommand(
                    command
                )
                // console.log(parsedCommand.value.where.conditions)
                expect(parsedCommand.value).toBeDefined()
                expect(parsedCommand.value.fields).toBeDefined()
                expect(parsedCommand.error).not.toBeDefined()
            })
        })
    }
)

//TODO
describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE hinta+1=!5;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta**2=20;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*+hinta>=20;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*--h<=20;',
])(
    'Invalid SELECT with WHERE condition containing arithmetic expression',
    (invalidCommand) => {
        const command = cleanCommand(invalidCommand)

        describe(invalidCommand, () => {
            test('fails validation', () => {
                const parsedCommand = selectAdvancedCommand.parseCommand(
                    command
                )
                // expect(parsedCommand.value).not.toBeDefined()
                // expect(parsedCommand.value.fields).not.toBeDefined()
                // expect(parsedCommand.error).toBeDefined()
            })
        })
    }
)

describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE hinta+1=5 AND 2*hinta<10;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*2=20 OR hinta*2=10;',
    'SELECT nimi, hinta FROM Taulu WHERE (hinta*hinta>=20 AND hinta < 15) OR hinta+1=6;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*h<=20 OR LENGTH(nimi)=8;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta/2=10 AND LENGTH(nimi)=8;',
])(
    'Valid SELECT with WHERE condition containing AND and OR operators',
    (validCommand) => {
        const command = cleanCommand(validCommand)

        describe(validCommand, () => {
            test('is valid and parsed', () => {
                const parsedCommand = selectAdvancedCommand.parseCommand(
                    command
                )
                // console.log(parsedCommand.value.where.conditions)
                expect(parsedCommand.value).toBeDefined()
                expect(parsedCommand.value.fields).toBeDefined()
                expect(parsedCommand.error).not.toBeDefined()
            })
        })
    }
)

//TODO
describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE hinta+1=5 AND 2*hinta<10;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*2=20 OR hinta*2=10;',
    'SELECT nimi, hinta FROM Taulu WHERE (hinta*hinta>=20 AND hinta < 15) OR hinta+1=6;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*h<=20 OR LENGTH(nimi)=8;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta/2=10 AND LENGTH(nimi)=8;',
])(
    'Invalid SELECT with WHERE condition containing AND and OR operators',
    (invalidCommand) => {
        const command = cleanCommand(invalidCommand)

        describe(invalidCommand, () => {
            test('fails validation', () => {
                const parsedCommand = selectAdvancedCommand.parseCommand(
                    command
                )
                // expect(parsedCommand.value).not.toBeDefined()
                // expect(parsedCommand.value.fields).not.toBeDefined()
                // expect(parsedCommand.error).toBeDefined()
            })
        })
    }
)
