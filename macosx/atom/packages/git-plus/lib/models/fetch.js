// @flow
import ListView from '../views/list-view'
import Repository from '../repository'
import type { FetchOptions } from '../repository'
import ActivityLogger from '../activity-logger'

export default async (options: FetchOptions = { prune: false }) => {
  const repo = await Repository.getCurrent()
  const remotes = await repo.getRemoteNames()
  let chosenRemote
  if (remotes.length === 1) chosenRemote = remotes[0]
  else chosenRemote = await new ListView(remotes).result

  const result = await repo.fetch(chosenRemote, options)
  ActivityLogger.record({
    repoName: repo.getName(),
    message: `fetch ${options.prune ? '--prune' : ''} from ${chosenRemote}`,
    ...result
  })
}
