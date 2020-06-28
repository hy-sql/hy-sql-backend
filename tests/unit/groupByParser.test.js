const Joi = require('@hapi/joi')
const { parseGroupBy } = require('../../commandParsers/groupByParser')
const {
    queryContainsGroupByKeyword,
    queryContainsWhereGroupByKeywords,
    queryContainsGroupByOrderByKeywords,
    queryContainsWhereGroupByOrderByKeywords,
    queryContainsGroupByHavingKeywords,
} = require('../../commandParsers/parserTools/queryContains')
const GroupBySchema = require('../../schemas/GroupBySchema')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')
const { parseCommand } = require('../../commandParsers/selectParser')

describe.each([
    'GROUP BY price',
    'GROUP by price*7',
    'group BY SUM(price)',
    'GROUP BY price, amount',
    "GROUP BY LENGTH('test')",
    'GROUP BY SUM(amount)',
    'GROUP BY price*5-LENGTH(name)',
])('Valid GROUP BY-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a GROUP BY keyword', () => {
            expect(queryContainsGroupByKeyword(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = Joi.attempt(
                parseGroupBy(command),
                GroupBySchema
            )

            expect(parsedCommand).toHaveProperty('keyword')
            expect(parsedCommand).toHaveProperty('fields')
            expect(parsedCommand.keyword).toBe('GROUP BY')
        })
    })
})

describe.each([
    'SELECT * FROM Table WHERE price>5 GROUP BY price;',
    "SELECT * FROM Table WHERE name='test' GROUP by price*7;",
    "SELECT * FROM Table WHERE price<10 AND name<>'sipuli' group BY SUM(price);",
    'SELECT * FROM Table WHERE LENGTH(name)=6 GROUP BY price, amount;',
    "SELECT * FROM Table WHERE amount=70 GROUP BY LENGTH('test');",
])('Valid WHERE GROUP BY-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a GROUP BY keyword', () => {
            expect(queryContainsWhereGroupByKeywords(command)).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('where')
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand.groupBy).toHaveProperty('keyword')
            expect(parsedCommand.groupBy).toHaveProperty('fields')
            expect(parsedCommand.groupBy.keyword).toBe('GROUP BY')
        })
    })
})

describe.each([
    'SELECT price, COUNT(*) FROM Table GROUP BY price HAVING COUNT(*)>1;',
    'SELECT price, COUNT(*) FROM Table GROUP by price HAVING COUNT(*)>1;',
    'SELECT price, name FROM Table GROUP by price HAVING price>5;',
])('Valid GROUP BY HAVING-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a GROUP BY and HAVING keywords', () => {
            expect(queryContainsGroupByHavingKeywords(command)).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand.groupBy).toHaveProperty('keyword')
            expect(parsedCommand.groupBy).toHaveProperty('fields')
            expect(parsedCommand.groupBy.keyword).toBe('GROUP BY')
            expect(parsedCommand).toHaveProperty('having')
            expect(parsedCommand.having).toHaveProperty('keyword')
            expect(parsedCommand.having.keyword).toBe('HAVING')
        })
    })
})

describe.each([
    'SELECT * FROM Table GROUP BY price order by price;',
    'SELECT * FROM Table GROUP by price*7 order by name;',
    'SELECT * FROM Table group BY SUM(price) order by amount;',
    'SELECT * FROM Table GROUP BY price, amount ORDER BY price;',
    'SELECT LENGTH(nimi) FROM Table GROUP BY LENGTH(nimi) ORDER BY LENGTH(nimi);',
])('Valid GROUP BY ORDER BY-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a GROUP BY keyword', () => {
            expect(queryContainsGroupByOrderByKeywords(command)).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand).toHaveProperty('orderBy')
            expect(parsedCommand.groupBy).toHaveProperty('keyword')
            expect(parsedCommand.groupBy).toHaveProperty('fields')
            expect(parsedCommand.groupBy.keyword).toBe('GROUP BY')
        })
    })
})

describe.each([
    'SELECT * FROM Table WHERE price>5 GROUP BY price order by price;',
    "SELECT * FROM Table WHERE name='test' GROUP by price*7 order by name;",
    "SELECT * FROM Table WHERE price<10 AND name<>'sipuli' group BY SUM(price) order by amount;",
    'SELECT * FROM Table WHERE LENGTH(name)=6 GROUP BY price, amount ORDER BY price;',
    'SELECT LENGTH(nimi) FROM Table WHERE amount>30 GROUP BY LENGTH(nimi) ORDER BY LENGTH(nimi);',
])('Valid WHERE GROUP BY ORDER BY-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a WHERE GROUP BY ORDER BY keyword', () => {
            expect(
                queryContainsWhereGroupByOrderByKeywords(command)
            ).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand).toHaveProperty('orderBy')
            expect(parsedCommand.groupBy).toHaveProperty('keyword')
            expect(parsedCommand.groupBy).toHaveProperty('fields')
            expect(parsedCommand.groupBy.keyword).toBe('GROUP BY')
        })
    })
})

describe.each([
    'SELECT * FROM Table WHERE price>5 order by price GROUP BY price;',
    "SELECT * FROM Table GROUP by price*7 WHERE name='test' order by name;",
    "SELECT * FROM Table WHERE price<10 AND name<>'sipuli' group BY SUM(price) order by;",
    'SELECT * FROM Table WHERE LENGTH(name)=6 ORDER BY price GROUP BY price, amount;', //TODO: Do we have to validate columns not to include sql keywords?
    'SELECT LENGTH(nimi) FROM Table WHERE amount>30 GROUP BY ORDER BY LENGTH(nimi);',
])('Invalid WHERE GROUP BY ORDER BY-part of a query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('fails validation after parsed to command object', () => {
            expect(() => {
                parseCommand(command)
            }).toThrowError()
        })
    })
})
