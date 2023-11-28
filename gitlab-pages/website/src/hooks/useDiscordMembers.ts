import { useEffect, useState } from "react";

const DEFAULT_MEMBERS = 11253;

export default function useDiscordMembers() {
  const [discordMembers, setDiscordMembers] = useState(DEFAULT_MEMBERS);

  useEffect(() => {
    const abortController = new AbortController();

    fetch("https://discord.com/api/v9/invites/tezos?with_counts=true", {
      signal: abortController.signal,
    })
      .then((response) => response.json())
      .then((data) => {
        const { approximate_member_count } = data;
        setDiscordMembers(approximate_member_count || DEFAULT_MEMBERS);
      });

    return () => {
      abortController.abort();
    };
  }, []);
  return discordMembers;
}
