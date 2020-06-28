const {
    queryContainsGroupByHavingKeywords,
    queryContainsWhereGroupByHavingKeywords,
    queryContainsGroupByHavingOrderByKeywords,
    queryContainsWhereGroupByHavingOrderByKeywords,
} = require('../../commandParsers/parserTools/queryContains')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')
const { parseCommand } = require('../../commandParsers/selectParser')

describe.each([
    'SELECT * FROM Table GROUP BY price HAVING AVG(price)>10;',
    'SELECT * FROM Table GROUP by price*7 HAVING COUNT(price)<10;',
    'SELECT * FROM Table group BY SUM(price) HAVING sum(price)<>3;',
    'SELECT * FROM Table GROUP BY price, amount HAVING MIN(price)<=100;',
    "SELECT * FROM Table GROUP BY LENGTH('test') HAVING LENGTH('test')>=5;",
    'SELECT * FROM Table GROUP BY SUM(amount) HAVING SUM(amount)<1000;',
])('Valid GROUP BY HAVING-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a HAVING keyword', () => {
            expect(queryContainsGroupByHavingKeywords(command)).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand).toHaveProperty('having')
            expect(parsedCommand.having).toHaveProperty('keyword')
            expect(parsedCommand.having).toHaveProperty('conditions')
            expect(parsedCommand.having.keyword).toBe('HAVING')
        })
    })
})

describe.each([
    'SELECT * FROM Table WHERE price>5 GROUP BY price HAVING AVG(price)>10;',
    "SELECT * FROM Table WHERE name='test' GROUP by price*7 HAVING COUNT(price)<10;",
    "SELECT * FROM Table WHERE price<10 AND name<>'sipuli' group BY SUM(price) HAVING sum(price)<>3;",
    'SELECT * FROM Table WHERE LENGTH(name)=6 GROUP BY price, amount HAVING MIN(price)<=100;',
    "SELECT * FROM Table WHERE amount=70 GROUP BY LENGTH('test') HAVING LENGTH('test')>=5;",
])('Valid WHERE GROUP BY HAVING-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a HAVING keyword', () => {
            expect(
                queryContainsWhereGroupByHavingKeywords(command)
            ).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('where')
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand).toHaveProperty('having')
            expect(parsedCommand.having).toHaveProperty('keyword')
            expect(parsedCommand.having).toHaveProperty('conditions')
            expect(parsedCommand.having.keyword).toBe('HAVING')
        })
    })
})

describe.each([
    'SELECT * FROM Table GROUP BY price HAVING AVG(price)>10 order by price;',
    'SELECT * FROM Table GROUP by price*7 HAVING COUNT(price)<10 order by name;',
    'SELECT * FROM Table group BY SUM(price) HAVING sum(price)<>3 order by amount;',
    'SELECT * FROM Table GROUP BY price, amount HAVING MIN(price)<=100 ORDER BY price;',
    'SELECT LENGTH(nimi) FROM Table GROUP BY LENGTH(nimi) HAVING LENGTH(nimi)>10 ORDER BY LENGTH(nimi);',
])('Valid GROUP BY HAVING ORDER BY-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a HAVING keyword', () => {
            expect(
                queryContainsGroupByHavingOrderByKeywords(command)
            ).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand).toHaveProperty('having')
            expect(parsedCommand).toHaveProperty('orderBy')
            expect(parsedCommand.having).toHaveProperty('keyword')
            expect(parsedCommand.having).toHaveProperty('conditions')
            expect(parsedCommand.having.keyword).toBe('HAVING')
        })
    })
})

describe.each([
    'SELECT * FROM Table WHERE price>5 GROUP BY price HAVING AVG(price)>10 order by price;',
    "SELECT * FROM Table WHERE name='test' GROUP by price*7 HAVING COUNT(price)<10 order by name;",
    "SELECT * FROM Table WHERE price<10 AND name<>'sipuli' group BY SUM(price) HAVING sum(price)<>3 order by amount;",
    'SELECT * FROM Table WHERE LENGTH(name)=6 GROUP BY price, amount HAVING MIN(price)<=100 ORDER BY price;',
    'SELECT LENGTH(nimi) FROM Table WHERE amount>30 GROUP BY LENGTH(nimi) HAVING LENGTH(nimi)>10 ORDER BY LENGTH(nimi);',
])('Valid WHERE GROUP BY ORDER BY-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a HAVING keyword', () => {
            expect(
                queryContainsWhereGroupByHavingOrderByKeywords(command)
            ).toBeTruthy()
        })

        const parsedCommand = parseCommand(command)

        test('is parsed and validated succesfully', () => {
            expect(parsedCommand).toHaveProperty('groupBy')
            expect(parsedCommand).toHaveProperty('having')
            expect(parsedCommand).toHaveProperty('orderBy')
            expect(parsedCommand.having).toHaveProperty('keyword')
            expect(parsedCommand.having).toHaveProperty('conditions')
            expect(parsedCommand.having.keyword).toBe('HAVING')
        })
    })
})

describe.each([
    'SELECT * FROM Table WHERE price>5 order by price GROUP BY price HAVING AVG(price)>10;',
    "SELECT * FROM Table WHERE price<10 AND name<>'sipuli' group BY SUM(price) having sum(price)<>3 order by;",
    'SELECT * FROM Table WHERE LENGTH(name)=6 ORDER BY price GROUP BY price, amount HAVING MIN(price)<=100;',
    'SELECT LENGTH(nimi) FROM Table WHERE amount>30 GROUP BY LENGTH(nimi) HAVING ORDER BY LENGTH(nimi);',
])(
    'Invalid WHERE GROUP BY HAVING ORDER BY-part of a query',
    (invalidCommand) => {
        describe(invalidCommand, () => {
            const command = splitCommandIntoArray(invalidCommand)

            test('fails validation after parsed to command object', () => {
                expect(() => {
                    parseCommand(command)
                }).toThrowError()
            })
        })
    }
)
