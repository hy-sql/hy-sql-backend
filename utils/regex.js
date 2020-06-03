const constraintsPatternForSplit = new RegExp(
    [
        '(?<=PRIMARY KEY)|(?=PRIMARY KEY)',
        '(?<=CHECK)|(?=CHECK)',
        '(?<=NOT NULL)|(?=NOT NULL)',
        '(?<=UNIQUE)|(?=UNIQUE)',
        '(?<=FOREIGN KEY)|(?=FOREIGN KEY)',
        '(?<=INDEX)|(?=INDEX)',
    ].join('|')
)

const constraintsPattern = new RegExp(
    ['CHECK', 'NOT NULL', 'UNIQUE', 'PRIMARY KEY', 'FOREIGN KEY', 'INDEX'].join(
        '|'
    )
)

const arithmeticOperatorPattern = RegExp('[+|-|*|/|%]', 'g')

const arithmeticExpressionPattern = /^\w+(( )?[+-/*%]{1}( )?\w+)+$/

const comparisonOperatorPattern = RegExp('[=|>|<|>=|<=|<>]')

const logicalOperatorPattern = RegExp(
    '[ALL|AND|ANY|BETWEEN|EXISTS|IN|LIKE|NOT|OR|SOME]',
    'g'
)

const stringFunctions = ['LENGTH', 'CONCAT', 'SUBSTRING']

const stringFunctionsPattern = new RegExp(stringFunctions.join('|'), 'i')

const stringFunctionExpressionPattern = new RegExp(
    `^(${stringFunctions.join('|')})\\(\\w+\\)$`,
    'i'
)

const aggregateFunctions = ['AVG', 'COUNT', 'MAX', 'MIN', 'SUM']

const aggregateFunctionsPattern = new RegExp(aggregateFunctions.join('|'), 'i')

const aggregateFunctionExpressionPattern = new RegExp(
    `^(${aggregateFunctions.join('|')})\\(\\w+\\)$|\\(\\*\\)$`,
    'i'
)

module.exports = {
    constraintsPatternForSplit,
    constraintsPattern,
    arithmeticOperatorPattern,
    arithmeticExpressionPattern,
    stringFunctionsPattern,
    stringFunctionExpressionPattern,
    aggregateFunctionsPattern,
    aggregateFunctionExpressionPattern,
    comparisonOperatorPattern,
    logicalOperatorPattern,
}
