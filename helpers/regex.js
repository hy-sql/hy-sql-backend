// ARRAYS TO USE WITH RegExp

const constraints = [
    'CHECK',
    'NOT NULL',
    'UNIQUE',
    'PRIMARY KEY',
    'FOREIGN KEY',
    'INDEX',
]

const comparisonOperators = ['>=', '<=', '<>', '=', '>', '<']

const stringFunctions = ['LENGTH', 'CONCAT', 'SUBSTRING']

const aggregateFunctions = ['AVG', 'COUNT', 'MAX', 'MIN', 'SUM']

const allFunctions = stringFunctions.concat(aggregateFunctions)

// Not in use
const logicalOperators = [
    'ALL',
    'AND',
    'ANY',
    'BETWEEN',
    'EXISTS',
    'IN',
    'LIKE',
    'NOT',
    'OR',
    'SOME',
]

// CONTAINS NAME MATCHES

const constraintsNamePattern = new RegExp(constraints.join('|'), 'i')

// Not in use
const logicalOperatorsNamePattern = new RegExp(logicalOperators.join('|'), 'i')

const allFunctionsNamePattern = new RegExp(allFunctions.join('|'), 'i')

const stringFunctionsNamePattern = new RegExp(stringFunctions.join('|'), 'i')

const aggregateFunctionsNamePattern = new RegExp(
    aggregateFunctions.join('|'),
    'i'
)

// EXACT MATCHES

const selectAllPattern = new RegExp('^\\*$')

// For Joi validation
const arithmeticOperatorPattern = new RegExp('^[+\\-*/%]$')

// For fieldParser
// Switched '*' sign to '**' doublesign, because of conflict with select operator '*'
const modifiedArithmeticOperatorPattern = new RegExp('^(\\+|-|\\/|\\*\\*|\\%)$')

const comparisonOperatorPattern = new RegExp(
    `(^${comparisonOperators.join('|')}$)`
)

const stringFunctionPattern = new RegExp(
    `^(${stringFunctions.join('|')})\\(('?\\w+'?|\\*)\\)$`,
    'i'
)

const aggregateFunctionPattern = new RegExp(
    `^(${aggregateFunctions.join(
        '|'
    )})\\((\\w+|\\*|\\w+(([+|\\-|*|/|%])\\w+)+)\\)$`,
    'i'
)

const functionPattern = new RegExp(
    `(^(${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\(('?\\w+'?|\\*)\\)$)`,
    'i'
)

const arithmeticExpressionPattern = new RegExp(
    `^(?!\\+|-|\\/|\\*\\*|\\%)(\\w+|(${stringFunctions.join(
        '|'
    )}|${aggregateFunctions.join(
        '|'
    )})\\(('?\\w+'?|\\*)\\))(([+|\\-|*|/|%])(\\w+|(${stringFunctions.join(
        '|'
    )}|${aggregateFunctions.join(
        '|'
    )})\\(('?\\w+'?|\\*)\\)))+(?!\\+|-|\\/|\\*\\*|\\%)$`
)

const textTypeInputPattern = new RegExp("^'.+'$")

const fieldsSplitByCommaPattern = new RegExp('^\\*$|(^(\\S+,\\s)*\\S+$)', 'i')

const sortOrderKeywordPattern = new RegExp('^(ASC|DESC)$', 'i')

const distinctKeywordPattern = new RegExp('^DISTINCT$', 'i')

// CONTAINS MATCHES

const containsArithmeticOperatorPattern = new RegExp('([\\+|\\-|*|/|%])', 'g')

const containsArithmeticOperatorWithWhiteSpacePattern = new RegExp(
    '\\s*([\\+|\\-|*|/|%])\\s*',
    'g'
)

const containsComparisonOperatorPattern = new RegExp(
    `(${comparisonOperators.join('|')})`
)

const containsComparisonOperatorWithWhiteSpacePattern = new RegExp(
    `\\s*(${comparisonOperators.join('|')})\\s*`,
    'g'
)

const containsStringFunctionPattern = new RegExp(
    `((${stringFunctions.join('|')})\\((\\w+|\\*)\\))`,
    'gi'
)

const containsAggregateFunctionPattern = new RegExp(
    `((${aggregateFunctions.join('|')})\\((\\w+|\\*)\\))`,
    'gi'
)

const containsFunctionPattern = new RegExp(
    `((${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\((\\w+|\\*)\\))`,
    'gi'
)

const containsFunctionWithWhiteSpacesPattern = new RegExp(
    `((${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\s*\\(\\s*('?\\w+'?|\\*)\\s*\\))`,
    'gi'
)

const containsArithmeticExpressionPattern = new RegExp(
    `(?!\\+|-|\\/|\\*\\*|\\%)(\\w+|(${stringFunctions.join(
        '|'
    )}|${aggregateFunctions.join(
        '|'
    )})\\(('?\\w+'?|\\*)\\))(([+|\\-|*|/|%])(\\w+|(${stringFunctions.join(
        '|'
    )}|${aggregateFunctions.join(
        '|'
    )})\\(('?\\w+'?|\\*)\\)))+(?!\\+|-|\\/|\\*\\*|\\%)`,
    'gi'
)

// TODO: improve this to match the ones above
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

/**
 * Regular expressions for use in the application.
 * @exports RegExp
 */
module.exports = {
    aggregateFunctionPattern,
    aggregateFunctionsNamePattern,
    allFunctionsNamePattern,
    arithmeticExpressionPattern,
    arithmeticOperatorPattern,
    comparisonOperatorPattern,
    constraintsNamePattern,
    constraintsNamePatternForSplit,
    containsAggregateFunctionPattern,
    containsArithmeticExpressionPattern,
    containsArithmeticOperatorPattern,
    containsArithmeticOperatorWithWhiteSpacePattern,
    containsComparisonOperatorPattern,
    containsComparisonOperatorWithWhiteSpacePattern,
    containsFunctionPattern,
    containsFunctionWithWhiteSpacesPattern,
    containsStringFunctionPattern,
    distinctKeywordPattern,
    fieldsSplitByCommaPattern,
    functionPattern,
    logicalOperatorsNamePattern,
    modifiedArithmeticOperatorPattern,
    selectAllPattern,
    sortOrderKeywordPattern,
    stringFunctionPattern,
    stringFunctionsNamePattern,
    textTypeInputPattern,
}
