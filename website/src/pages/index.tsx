import clsx from 'clsx';
import Heading from '@theme/Heading';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';

import styles from './index.module.css';

type Feature = {
  title: string;
  description: string;
  to: string;
};

const features: Feature[] = [
  {
    title: 'Learn the Language',
    description: 'Follow a didactic manual: values, functions, control flow, and pattern matching.',
    to: '/manual/learn/quickstart',
  },
  {
    title: 'Explore Examples',
    description: 'Study practical scripts and reusable patterns you can adapt immediately.',
    to: '/manual/examples/guided-examples',
  },
  {
    title: 'Embed in .NET Hosts',
    description: 'Expose host capabilities through externs with explicit typing and safety boundaries.',
    to: '/manual/embedding/overview',
  },
];

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={styles.hero}>
      <div className="container">
        <Heading as="h1" className={styles.title}>
          <span className={styles.brandRow}>
            <img
              className={styles.heroLogo}
              src="/FScript/img/fscript-icon.png"
              alt="FScript logo"
              width={56}
              height={56}
            />
            <span>{siteConfig.title}</span>
          </span>
        </Heading>
        <p className={styles.tagline}>{siteConfig.tagline}</p>
        <div className={styles.actions}>
          <Link className="button button--primary button--lg" to="/manual/learn/quickstart">
            Start Tutorial
          </Link>
          <Link className="button button--secondary button--lg" to="/manual/language/values-and-bindings">
            Read the Manual
          </Link>
          <Link className="button button--secondary button--lg" to="/manual/examples/guided-examples">
            Browse Examples
          </Link>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout title={siteConfig.title} description={siteConfig.tagline}>
      <HomepageHeader />
      <main className={styles.main}>
        <section className="container">
          <div className={styles.grid}>
            {features.map((feature) => (
              <article key={feature.title} className={clsx('card', styles.card)}>
                <div className="card__body">
                  <Heading as="h2">{feature.title}</Heading>
                  <p>{feature.description}</p>
                </div>
                <div className="card__footer">
                  <Link to={feature.to}>Open</Link>
                </div>
              </article>
            ))}
          </div>
        </section>
      </main>
    </Layout>
  );
}
