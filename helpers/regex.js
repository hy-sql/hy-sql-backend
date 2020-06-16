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

// Switched '*' sign to '**' doublesign, because of conflict with select operator '*'
const isArithmeticOperator = new RegExp('^(\\+|-|\\/|\\*\\*|\\%)$')

const arithmeticOperatorPattern = new RegExp('([\\+|\\-|*|/|%])', 'g')

const arithmeticExpressionPattern = new RegExp('.+([+|\\-|*|/|%]).+')

const comparisonOperators = ['>=', '<=', '<>', '=', '>', '<']

const comparisonOperatorPattern = RegExp('(>=|<=|<>|=|>|<)')

const logicalOperatorsNamePattern = RegExp(
    '[ALL|AND|ANY|BETWEEN|EXISTS|IN|LIKE|NOT|OR|SOME]',
    'g'
)

const stringFunctions = ['LENGTH', 'CONCAT', 'SUBSTRING']

const aggregateFunctions = ['AVG', 'COUNT', 'MAX', 'MIN', 'SUM']

const allFunctions = stringFunctions.concat(aggregateFunctions)

const stringFunctionsNamePattern = new RegExp(stringFunctions.join('|'), 'i')

const stringFunctionPattern = new RegExp(
    `^(${stringFunctions.join('|')})\\(('?\\w+'?|\\*)\\)$`,
    'i'
)

const containsStringFunctionPattern = new RegExp(
    `((${stringFunctions.join('|')})\\((\\w+|\\*)\\))`,
    'i'
)

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

const functionExpressionPattern = new RegExp(
    `((${allFunctions.join('|')})\\(\\w+\\)(${comparisonOperators.join(
        '|'
    )})\\w+)|(\\w+(<)$({allFunctions.join(
        '|'
    )})\\(\\w+\\))`
)

const containsFunctionPattern = new RegExp(
    `((${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\((\\w+|\\*)\\))`,
    'gi'
)

const sortOrderKeywordPattern = new RegExp('^(ASC|DESC)$', 'i')

const distinctKeywordPattern = new RegExp('DISTINCT')

module.exports = {
    constraintsNamePatternForSplit,
    constraintsNamePattern,
    isArithmeticOperator,
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
    functionExpressionPattern,
    containsFunctionPattern,
    sortOrderKeywordPattern,
    distinctKeywordPattern,
}
