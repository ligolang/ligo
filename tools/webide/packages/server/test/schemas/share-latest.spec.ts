import latestSchema from '../../src/schemas/share-latest';

describe('Latest Share Schema Migration', () => {
  it('should be v3', () => {
    expect(latestSchema.VERSION).toEqual('v3');
  });
});
