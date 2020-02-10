import { Request, Response } from 'express';

import { loadDefaultState } from '../load-state';
import latestSchema from '../schemas/share-latest';
import { storage } from '../storage';
import { FileNotFoundError } from '../storage/interface';
import { logger } from '../logger';

export function createSharedLinkHandler(
  appBundleDirectory: string,
  template: (state: string) => string
) {
  return async (req: Request, res: Response) => {
    try {
      const content = await storage.read(`${req.params['hash']}.txt`);
      const storedState = JSON.parse(content);
      const migratedState = latestSchema.forward(storedState);
      const defaultState = await loadDefaultState(appBundleDirectory);

      const state = {
        ...defaultState,
        ...migratedState.state,
        share: { link: req.params['hash'] }
      };

      res.send(template(JSON.stringify(state)));
    } catch (ex) {
      if (ex instanceof FileNotFoundError) {
        res.sendStatus(404);
      } else {
        logger.error(ex);
        res.sendStatus(500);
      }
    }
  };
}
