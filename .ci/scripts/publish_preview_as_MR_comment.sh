#!/usr/bin/env bash
CI_PROJECT_ID=$1
CI_MERGE_REQUEST_IID=$2
GITLAB_PERSONAL_API_PRIVATE_TOKEN=$3
MESSAGE=$4
USER_DISCRIMINATOR=$5
MESSAGE_DISCRIMINATOR=$6

COMMENT_DISCRIMINATOR_REGEX=".*$MESSAGE_DISCRIMINATOR.*"

COMMENTS_JSON=$(curl  --header "Private-Token: $GITLAB_PERSONAL_API_PRIVATE_TOKEN" https://gitlab.com/api/v4/projects/$CI_PROJECT_ID/merge_requests/$CI_MERGE_REQUEST_IID/notes?sort=desc&order_by=updated_at)
EXISTING_PREVIEW_COMMENT=$(jq "[.[] | select(.author.username==\"$USER_DISCRIMINATOR\") | select(.body|test(\".*$MESSAGE_DISCRIMINATOR.*\"))][0] | .id" <<< $COMMENTS_JSON)
echo $EXISTING_PREVIEW_COMMENT

if [[ $EXISTING_PREVIEW_COMMENT != null ]]; then
  curl -X PUT --header "Content-Type: application/json" --header "Private-Token: $GITLAB_PERSONAL_API_PRIVATE_TOKEN" https://gitlab.com/api/v4/projects/$CI_PROJECT_ID/merge_requests/$CI_MERGE_REQUEST_IID/notes/$EXISTING_PREVIEW_COMMENT  -d "{\"body\": \"$MESSAGE\"}"
else
  curl -X POST  --header "Content-Type: application/json" --header "Private-Token: $GITLAB_PERSONAL_API_PRIVATE_TOKEN" https://gitlab.com/api/v4/projects/$CI_PROJECT_ID/merge_requests/$CI_MERGE_REQUEST_IID/notes -d "{\"body\": \"$MESSAGE\"}"
fi
