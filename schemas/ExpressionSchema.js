const Joi = require('@hapi/joi')
const {
    ArithmeticOperatorSchema,
    ColumnSchema,
    IntegerSchema,
} = require('./FieldSchemas')
const { arithmeticExpressionPattern } = require('../helpers/regex')

/**
 * Joi schema for validating arithmetic expressions.
 */
const ExpressionSchema = Joi.object({
    type: Joi.string().valid('expression').required(),
    expressionParts: Joi.array()
        .items(
            ArithmeticOperatorSchema.required(),
            ColumnSchema,
            IntegerSchema,
            Joi.link('#function')
        )
        .required(),
    value: Joi.string().pattern(arithmeticExpressionPattern).required(),
}).id('expression')

module.exports = ExpressionSchema
