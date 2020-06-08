const constraintsNamePatternForSplit = new RegExp(
    [
        '(?<=PRIMARY KEY)|(?=PRIMARY KEY)',
        '(?<=CHECK)|(?=CHECK)',
        '(?<=NOT NULL)|(?=NOT NULL)',
        '(?<=UNIQUE)|(?=UNIQUE)',
        '(?<=FOREIGN KEY)|(?=FOREIGN KEY)',
        '(?<=INDEX)|(?=INDEX)',
    ].join('|')
)

const constraintsNamePattern = new RegExp(
    ['CHECK', 'NOT NULL', 'UNIQUE', 'PRIMARY KEY', 'FOREIGN KEY', 'INDEX'].join(
        '|'
    )
)

const arithmeticOperatorPattern = new RegExp('([+|\\-|*|/|%])', 'g')

const arithmeticExpressionPattern = new RegExp('.+([+|\\-|*|/|%]).+')

const comparisonOperatorPattern = RegExp('(>=|<=|<>|=|>|<)')

const logicalOperatorsNamePattern = RegExp(
    '[ALL|AND|ANY|BETWEEN|EXISTS|IN|LIKE|NOT|OR|SOME]',
    'g'
)

const stringFunctions = ['LENGTH', 'CONCAT', 'SUBSTRING']

const stringFunctionsNamePattern = new RegExp(stringFunctions.join('|'), 'i')

const stringFunctionPattern = new RegExp(
    `^(${stringFunctions.join('|')})\\((\\w+|\\*)\\)$`,
    'i'
)

const containsStringFunctionPattern = new RegExp(
    `((${stringFunctions.join('|')})\\((\\w+|\\*)\\))`,
    'i'
)

const aggregateFunctions = ['AVG', 'COUNT', 'MAX', 'MIN', 'SUM']

const aggregateFunctionsNamePattern = new RegExp(
    aggregateFunctions.join('|'),
    'i'
)

const aggregateFunctionPattern = new RegExp(
    `^(${aggregateFunctions.join('|')})\\((\\w+|\\*)\\)$`,
    'i'
)

const containsAggregateFunctionPattern = new RegExp(
    `((${aggregateFunctions.join('|')})\\((\\w+|\\*)\\))`,
    'i'
)

const isFunctionPattern = new RegExp(
    `((${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\((\\w+|\\*)\\))`,
    'i'
)

module.exports = {
    constraintsNamePatternForSplit,
    constraintsNamePattern,
    arithmeticOperatorPattern,
    arithmeticExpressionPattern,
    stringFunctionsNamePattern,
    stringFunctionPattern,
    aggregateFunctionsNamePattern,
    aggregateFunctionPattern,
    comparisonOperatorPattern,
    logicalOperatorsNamePattern,
    containsStringFunctionPattern,
    containsAggregateFunctionPattern,
    isFunctionPattern,
}
