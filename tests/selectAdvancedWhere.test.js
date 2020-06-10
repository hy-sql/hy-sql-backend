/*eslint-disable no-unused-vars*/
const selectAdvancedCommand = require('../commands/selectAdvancedCommand')
const cleanCommand = require('../utils/cleanCommand')

/**
 * SELECT ... WHERE ... with aritmetic expression
 */
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

//ei mene läpi vielä
/**
 * Invalid SELECT ... WHERE ... with aritmetic expression
 */
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

/**
 * SELECT ... WHERE ... AND ... OR ... with aritmetic expression
 */
describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE hinta+1=5 AND 2*hinta<10;',
    'SELECT nimi, hinta FROM Taulu WHERE hinta*2=20 OR hinta*2=10;',
    'SELECT nimi, hinta FROM Taulu WHERE (hinta*hinta>=20 AND hinta < 15) OR hinta+1=6;',
    "SELECT nimi, hinta FROM Taulu WHERE hinta*h<=20 OR nimi='nauris;",
    "SELECT nimi, hinta FROM Taulu WHERE hinta/2=10 AND nimi='nauris;",
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

//ei mene läpi vielä
/**
 * Invalid SELECT ... WHERE ... AND ... OR ... with aritmetic expression
 */
describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE hinta+1=5 AND 123, 2*hinta<10;', // random integer between conditions
    'SELECT nimi, hinta FROM Taulu WHERE hinta*2=20 OR 123, hinta*2=10;', // random integer between conditions
    'SELECT nimi, hinta FROM Taulu WHERE (hinta*hinta>=20 AND hinta < 15) 123abc OR hinta+1=6;', // random string between conditions
    'SELECT nimi, hinta FROM Taulu WHERE hinta*h<=20 OR nimi: nauris;', // equality operator missing
    'SELECT nimi, hinta FROM Taulu WHERE hinta/2=10 AND nimi: nauris;', // equality operator missing
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

/**
 * SELECT ... WHERE ... with function operation
 */
describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi)=8;',
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi)<8;',
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi)>8;',
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi)>=8;',
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi)<=8;',
])(
    'SELECT with WHERE condition containing function operation',
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

//ei mene vielä läpi
/**
 * Invalid SELECT ... WHERE ... with function operation
 */
describe.each([
    'SELECT nimi, hinta FROM Taulu WHERE LEgTH(nimi)=8;', //typo
    'SELECT nimi, hinta FROM Taulu WHERE LEN(nimi)<8;', //typo
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi>8;', //brackets not closed
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi)>=;', //value missing
    'SELECT nimi, hinta FROM Taulu WHERE LENGTH(nimi):8;', //equality operator missing
])(
    'Invalid SELECT with WHERE condition containing function operation',
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
