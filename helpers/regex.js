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

const arithmeticOperator = new RegExp('^[+\\-*/%]$')

// Switched '*' sign to '**' doublesign, because of conflict with select operator '*'
const modifiedArithmeticOperator = new RegExp('^(\\+|-|\\/|\\*\\*|\\%)$')

const containsArithmeticOperatorPattern = new RegExp('([\\+|\\-|*|/|%])', 'g')

const comparisonOperators = ['>=', '<=', '<>', '=', '>', '<']

const comparisonOperatorPattern = RegExp('(>=|<=|<>|=|>|<)')

const arithmeticOperatorPatternWithWhiteSpace = RegExp(
    '\\s*([\\+|\\-|*|/|%])\\s*',
    'g'
)

const comparisonOperatorPatternWithWhiteSpace = RegExp(
    '\\s*(>=|<=|<>|=|>|<)\\s*',
    'g'
)

const conditionPattern = new RegExp(
    `\\w+${comparisonOperators.join('|')}(\\w+)|'`
)

const logicalOperatorsNamePattern = RegExp(
    '[ALL|AND|ANY|BETWEEN|EXISTS|IN|LIKE|NOT|OR|SOME]',
    'g'
)

const stringFunctions = ['LENGTH', 'CONCAT', 'SUBSTRING']

const aggregateFunctions = ['AVG', 'COUNT', 'MAX', 'MIN', 'SUM']

const allFunctions = stringFunctions.concat(aggregateFunctions)

const allFunctionsNamePattern = new RegExp(allFunctions.join('|'))

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
    `^(${aggregateFunctions.join(
        '|'
    )})\\((\\w+|\\*|\\w+(([+|\\-|*|/|%])\\w+)+)\\)$`,
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

const functionPattern = new RegExp(
    `(^(${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\(('?\\w+'?|\\*)\\)$)`,
    'i'
)

const containsFunctionPattern = new RegExp(
    `((${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\((\\w+|\\*)\\))`,
    'gi'
)

const containsFunctionPatternWithWhiteSpaces = new RegExp(
    `((${stringFunctions.join('|')}|${aggregateFunctions.join(
        '|'
    )})\\s*\\(\\s*('?\\w+'?|\\*)\\s*\\))`,
    'gi'
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

const textInputPattern = new RegExp("^'.+'$")

const sortOrderKeywordPattern = new RegExp('^(ASC|DESC)$', 'i')

const distinctKeywordPattern = new RegExp('^[Dd][Ii][Ss][Tt][Ii][Nn][Cc][Tt]$')

const fieldsSplitByCommaPattern = new RegExp('^\\*$|(^(\\S+,\\s)*\\S+$)', 'i')

/**
 * Regular expressions for use in the application.
 * @exports RegExp
 */
module.exports = {
    aggregateFunctionPattern,
    aggregateFunctionsNamePattern,
    allFunctionsNamePattern,
    arithmeticExpressionPattern,
    arithmeticOperator,
    comparisonOperatorPattern,
    constraintsNamePattern,
    constraintsNamePatternForSplit,
    containsAggregateFunctionPattern,
    containsArithmeticOperatorPattern,
    containsFunctionPattern,
    containsStringFunctionPattern,
    functionExpressionPattern,
    functionPattern,
    logicalOperatorsNamePattern,
    modifiedArithmeticOperator,
    sortOrderKeywordPattern,
    distinctKeywordPattern,
    stringFunctionPattern,
    stringFunctionsNamePattern,
    textInputPattern,
    comparisonOperatorPatternWithWhiteSpace,
    conditionPattern,
    containsFunctionPatternWithWhiteSpaces,
    fieldsSplitByCommaPattern,
    containsArithmeticExpressionPattern,
    arithmeticOperatorPatternWithWhiteSpace,
}
