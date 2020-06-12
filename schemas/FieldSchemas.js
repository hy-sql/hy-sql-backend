const Joi = require('@hapi/joi')
const {
    stringFunctionsNamePattern,
    stringFunctionPattern,
    // aggregateFunctionsNamePattern,
    // aggregateFunctionPattern,
} = require('../helpers/regex')

const AllFieldsSchema = Joi.object({
    type: Joi.string().valid('all').required(),
    value: Joi.string().valid('*').required(),
})

const ColumSchema = Joi.object({
    type: Joi.string().valid('column').required(),
    value: Joi.string().pattern(/^\w+$/).required().max(64),
})

const StringFunctionSchema = Joi.object({
    type: Joi.string().valid('stringFunction').required(),
    name: Joi.string().pattern(stringFunctionsNamePattern).required(),
    value: Joi.string().pattern(stringFunctionPattern).insensitive().required(),
    param: Joi.object().required(),
})

const TextSchema = Joi.object({
    type: Joi.string().valid('TEXT').required(),
    value: Joi.string().pattern(/^\w+$/).required().max(64),
})

const IntegerSchema = Joi.object({
    type: Joi.string().valid('INTEGER').required(),
    value: Joi.number().required().messages({
        'any.required':
            'value type without singlequotes is expected to be number',
    }),
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
    StringFunctionSchema,
    TextSchema,
    IntegerSchema,
    ArithmeticOperatorSchema,
    ArithmeticExpressionSchema,
    SortOrderKeyword,
    FunctionSchema,
}
