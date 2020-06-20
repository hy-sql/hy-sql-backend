const { parseGroupBy } = require('../../commandParsers/groupByParser')
const {
    queryContainsGroupByKeywords,
    queryContainsWhereGroupByKeywords,
    queryContainsGroupByOrderByKeywords,
    queryContainsWhereGroupByOrderByKeywords,
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
    // 'GROUP BY price*5-LENGTH(name)', FIXME: Implement expressions with functions
])('Valid GROUP BY-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a GROUP BY keyword', () => {
            expect(queryContainsGroupByKeywords(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = GroupBySchema.validate(parseGroupBy(command))

            expect(parsedCommand.value).toHaveProperty('keyword')
            expect(parsedCommand.value).toHaveProperty('fields')
            expect(parsedCommand.value.keyword).toBe('GROUP BY')
            expect(parsedCommand.error).toBeUndefined()
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
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.value).toHaveProperty('groupBy')
            expect(parsedCommand.value.groupBy).toHaveProperty('keyword')
            expect(parsedCommand.value.groupBy).toHaveProperty('fields')
            expect(parsedCommand.value.groupBy.keyword).toBe('GROUP BY')
            expect(parsedCommand.error).toBeUndefined()
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
            expect(parsedCommand.value).toHaveProperty('groupBy')
            expect(parsedCommand.value).toHaveProperty('orderBy')
            expect(parsedCommand.value.groupBy).toHaveProperty('keyword')
            expect(parsedCommand.value.groupBy).toHaveProperty('fields')
            expect(parsedCommand.value.groupBy.keyword).toBe('GROUP BY')
            expect(parsedCommand.error).toBeUndefined()
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
            expect(parsedCommand.value).toHaveProperty('groupBy')
            expect(parsedCommand.value).toHaveProperty('orderBy')
            expect(parsedCommand.value.groupBy).toHaveProperty('keyword')
            expect(parsedCommand.value.groupBy).toHaveProperty('fields')
            expect(parsedCommand.value.groupBy.keyword).toBe('GROUP BY')
            expect(parsedCommand.error).toBeUndefined()
        })
    })
})

describe.each([
    'SELECT * FROM Table WHERE price>5 order by price GROUP BY price;',
    // "SELECT * FROM Table GROUP by price*7 WHERE name='test' order by name;", //FIXME: Additional input after base select part before where
    "SELECT * FROM Table WHERE price<10 AND name<>'sipuli' group BY SUM(price) order by;",
    'SELECT * FROM Table WHERE LENGTH(name)=6 ORDER BY price GROUP BY price, amount;', //TODO: Do we have to validate columns not to include sql keywords?
    'SELECT LENGTH(nimi) FROM Table WHERE amount>30 GROUP BY ORDER BY LENGTH(nimi);',
])('Invalid WHERE GROUP BY ORDER BY-part of a query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand.error).toBeDefined()
        })
    })
})