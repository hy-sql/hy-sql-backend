const supertest = require('supertest')
const app = require('../app')
const api = supertest(app)

test('API endpoint responds to ping', async (done) => {
    await api
    .get('/api/ping')
    .expect(200)
    .expect('Content-Type', /application\/json/)
    done()
})
