const Joi = require('@hapi/joi')

const AllFieldsSchema = Joi.object({
    type: Joi.string(),
    value: Joi.string(),
})

const ColumSchema = Joi.object({
    type: Joi.string(),
    value: Joi.string(),
})

const TextSchema = Joi.object({
    type: Joi.string(),
    value: Joi.string(),
})

const IntegerSchema = Joi.object({
    type: Joi.string(),
    value: Joi.number(),
})

const ArithmeticOperatorSchema = Joi.object({
    type: Joi.string(),
    value: Joi.string(),
})

const ArithmeticExpressionSchema = Joi.object({
    type: Joi.string(),
    value: Joi.string(),
    stringValue: Joi.string(),
})

const SortOrderKeyword = Joi.object({
    type: Joi.string(),
    value: Joi.string(),
})

const FunctionSchema = Joi.object()

module.exports = {
    AllFieldsSchema,
    ColumSchema,
    TextSchema,
    IntegerSchema,
    ArithmeticOperatorSchema,
    ArithmeticExpressionSchema,
    SortOrderKeyword,
    FunctionSchema,
}
