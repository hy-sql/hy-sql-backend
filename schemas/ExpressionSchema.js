const Joi = require('@hapi/joi')
const {
    ArithmeticOperatorSchema,
    ColumnSchema,
    IntegerSchema,
} = require('./FieldSchemas')
const { arithmeticExpressionPattern } = require('../helpers/regex')

const ExpressionSchema = Joi.object({
    type: Joi.string().valid('expression').required(),
    value: Joi.array()
        .items(
            ArithmeticOperatorSchema.required(),
            ColumnSchema,
            IntegerSchema,
            Joi.link('#function')
        )
        .required(),
    stringValue: Joi.string().pattern(arithmeticExpressionPattern).required(),
}).id('expression')

module.exports = ExpressionSchema
