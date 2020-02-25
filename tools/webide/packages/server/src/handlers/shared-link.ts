import { Request, Response } from 'express';

import { loadDefaultState } from '../load-state';
import { logger } from '../logger';
import latestSchema from '../schemas/share-latest';
import { storage } from '../storage';
import { FileNotFoundError } from '../storage/interface';

export function sharedLinkHandler(
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
