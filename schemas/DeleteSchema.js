const Joi = require('@hapi/joi')
const WhereSchema = require('./WhereAdvancedSchema')

const DeleteSchema = Joi.object({
    name: Joi.string().required().valid('DELETE').insensitive().messages({
        'any.only': 'Query must begin with DELETE',
        'any.required': 'Query must begin with DELETE',
    }),

    from: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^[Ff][Rr][Oo][Mm]$/)
        .messages({
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base': 'DELETE must be followed by FROM',
            'any.required': 'DELETE must be followed by FROM',
        }),

    tableName: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .messages({
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'any.required': 'Query must contain a table name',
            'string.pattern.base':
                'Table name should only contain one or more alphanumeric characters and underscores',
            'string.max': 'Table name is too long',
        }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

const DeleteWhereSchema = DeleteSchema.keys({
    where: WhereSchema,
})

module.exports = {
    DeleteSchema,
    DeleteWhereSchema,
}
