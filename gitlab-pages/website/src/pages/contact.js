import React from 'react';
import Layout from '@theme/Layout';
import useBaseUrl from '@docusaurus/useBaseUrl';

const TEAM = [
  {
    firstName: 'Christian',
    lastName: 'Rinderknecht',
    image: 'img/christian.jpeg',
    link: 'https://github.com/rinderknecht',
    pinned: true
  },
  {
    firstName: 'Brice',
    lastName: 'Aldrich',
    image: 'img/brice.jpeg',
    link: 'https://github.com/DefinitelyNotAGoat',
    pinned: true
  },
  {
    firstName: 'Gabriel',
    lastName: 'Alfour',
    image: 'img/gabriel.jpeg',
    link: 'https://gitlab.com/gabriel.alfour',
    pinned: true
  },
  {
    firstName: 'Matej',
    lastName: 'Sima',
    image: 'img/matej.jpeg',
    link: 'https://github.com/maht0rz',
    pinned: true
  },
  {
    firstName: 'Sander',
    lastName: 'Spies',
    image: 'img/sander.jpeg',
    link: 'https://github.com/SanderSpies',
    pinned: true
  },
  {
    firstName: 'Suzanne',
    lastName: 'Dupéron',
    image: 'img/suzanne.jpeg',
    link: 'https://gitlab.com/suzanne.duperon',
    pinned: true
  }
];

const COMMUNICATION_CHANNELS = [
  {
    link: 'https://t.me/LigoLang',
    icon: 'img/telegram.svg',
    description: "We're here to help. Ask us anything"
  },
  {
    link: 'https://gitlab.com/ligolang/ligo/issues',
    icon: 'img/gitlab.svg',
    description: 'Need a fix? Create an issue on GitLab'
  },
  {
    link: 'https://twitter.com/ligolang',
    icon: 'img/twitter.svg',
    description: 'Join the latest chit-chat'
  }
];

const Portrait = (config, props) => {
  return (
    <a
      href={props.link}
      className="portraitContainer"
      key={props.link}
      target="_blank"
      rel="noopener noreferrer"
    >
      <img className="portrait" src={useBaseUrl(props.image)} />
      <div className="overlay">
        <span>{props.firstName}</span>
        <span>{props.lastName}</span>
      </div>
    </a>
  );
};

const CommunicationChannel = (config, props) => {
  return (
    <a
      className="option"
      href={props.link}
      target="_blank"
      rel="noopener noreferrer"
    >
      <img className="icon" src={useBaseUrl(props.icon)} />
      {props.description}
    </a>
  );
};

export default props => {
  const pinnedMembers = TEAM.filter(member => member.pinned);
  const membersCeilCount = Math.ceil(pinnedMembers.length / 2);
  const membersInFistColumn = pinnedMembers.slice(0, membersCeilCount);
  const membersInSecondColumn = pinnedMembers.slice(membersCeilCount);
   return <Layout title="Contact">
        <div
            id="contactPage"
            style={{
            display: 'flex',
            justifyContent: 'stretch',
            alignItems: 'stretch',
            fontSize: '20px',
            flexDirection: 'row'
            }}>
     <div id="mural">
        <div className="column">
        {membersInFistColumn.map(entry => Portrait(props.config, entry))}
        </div>
        <div className="offset column">
        {membersInSecondColumn.map(entry => Portrait(props.config, entry))}
        </div>
    </div>
    <div id="message">
        <div className="title">Talk to us</div>
        <div className="communicationOptions">
        {COMMUNICATION_CHANNELS.map(entry =>
            CommunicationChannel(props.config, entry)
        )}
        </div>
    </div>
    </div>
  </Layout>  
}

