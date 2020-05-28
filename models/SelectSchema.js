const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string().pattern(/^\w+$/).max(64).required().messages({
        'string.pattern.base':
            'One of the column names is invalid. Only a-z,A-Z,0-9 and _ allowed.',
    }),
})

const SelectSchema = Joi.object({
    name: Joi.string().required().valid('SELECT').insensitive().messages({
        'any.only': 'Query must begin with SELECT',
        'any.required': 'Query must begin with SELECT',
    }),

    size: Joi.number().positive().required().messages({}),

    parserCounter: Joi.number().required().min(Joi.ref('size')).messages({}),

    columns: Joi.array().min(1).items(ColumnsSchema).required().messages({
        'array.base': 'this is not an array',
        'array.min': 'there should be at least one column',
    }),

    from: Joi.string().required().valid('FROM').insensitive().messages({
        'any.only': 'SELECT * must be followed by FROM',
        'any.required': 'SELECT * must be followed by FROM',
    }),

    tableName: Joi.string().required().pattern(/^\w+$/).max(64).messages({
        'any.required': 'Query must contain a table name',
        'string.pattern.base':
            'Table name should only contain one or more alphanumeric characters and underscores',
    }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = { SelectSchema }
