import React, {useState} from 'react';
import Link from '@docusaurus/Link';
import Heading from '@theme/Heading';
import Layout from '@theme/Layout';
import useBaseUrl from '@docusaurus/useBaseUrl';

import styles from './index.module.css';

type DemoKey = 'script' | 'dotnet' | 'javascript';

const demos: Record<DemoKey, {label: string; filename: string; language: string; code: string}> = {
  script: {
    label: 'The script',
    filename: 'pricing.fss',
    language: 'FScript',
    code: `type Customer =
  { Name: string
    Plan: string
    Seats: int }

[<export>] let quote (customer: Customer) =
  let basePrice =
    match customer.Plan with
    | "scale" -> 29
    | "team" -> 19
    | _ -> 9

  basePrice * customer.Seats`,
  },
  dotnet: {
    label: '.NET host',
    filename: 'Pricing.fs',
    language: 'F#',
    code: `open System.IO
open FScript.Language
open FScript.Runtime

let source = File.ReadAllText "pricing.fss"
let externs = Registry.all hostContext
let pricing = ScriptHost.loadSource externs source

let customer =
  VRecord (Map.ofList
    [ "Name", VString "Acme"
      "Plan", VString "scale"
      "Seats", VInt 12L ])

let total =
  ScriptHost.invoke pricing "quote" [ customer ]
// Change the rule. Keep the host.`,
  },
  javascript: {
    label: 'JavaScript host',
    filename: 'pricing.mjs',
    language: 'JavaScript',
    code: `import { load, invoke } from
  "@magnusopera/fscript";

const pricing = load(source);

const total = invoke(pricing, "quote", [{
  Name: "Acme",
  Plan: "scale",
  Seats: 12
}]);

// The language runs inside your process.`,
  },
};

const examples = [
  {
    number: '01',
    title: 'Model the messy parts',
    description: 'Records, unions, options, and exhaustive matching make business rules readable instead of fragile.',
    code: `type Decision =\n  | Approve of int\n  | Review of string\n  | Decline\n\nlet decide score =\n  match score with\n  | n when n > 80 -> Approve 5000\n  | n when n > 55 -> Review "manual"\n  | _ -> Decline`,
  },
  {
    number: '02',
    title: 'Hand it capabilities',
    description: 'Scripts only reach the functions your host exposes. The integration boundary is explicit and typed.',
    code: `let notifyLateInvoice invoice =\n  match invoice.Status with\n  | "late" ->\n      Host.notify invoice.Owner\n        $"Invoice {invoice.Id} is late"\n  | _ ->\n      ()`,
  },
  {
    number: '03',
    title: 'Ship rules, not rebuilds',
    description: 'Load once, discover exports, and invoke them by name from .NET or JavaScript hosts.',
    code: `type Event = { Kind: string }\n\n[<export>] let route (event: Event) =\n  match event.Kind with\n  | "signup" -> "onboarding"\n  | "payment.failed" -> "billing"\n  | "churn.risk" -> "success"\n  | _ -> "archive"`,
  },
];

function Arrow() {
  return <span aria-hidden="true">↗</span>;
}

function DemoWindow() {
  const [activeDemo, setActiveDemo] = useState<DemoKey>('script');
  const demo = demos[activeDemo];

  return (
    <div className={styles.demoWindow}>
      <div className={styles.windowBar}>
        <div className={styles.windowDots} aria-hidden="true"><i /><i /><i /></div>
        <span className={styles.windowTitle}>{demo.filename}</span>
        <span className={styles.liveBadge}><i /> in-process</span>
      </div>
      <div className={styles.demoTabs} role="tablist" aria-label="Embedding example">
        {(Object.keys(demos) as DemoKey[]).map((key) => (
          <button
            key={key}
            type="button"
            role="tab"
            aria-selected={activeDemo === key}
            className={activeDemo === key ? styles.demoTabActive : styles.demoTab}
            onClick={() => setActiveDemo(key)}>
            {demos[key].label}
          </button>
        ))}
      </div>
      <div className={styles.codeArea}>
        <div className={styles.lineNumbers} aria-hidden="true">
          {demo.code.split('\n').map((_, index) => <span key={index}>{index + 1}</span>)}
        </div>
        <pre><code>{demo.code}</code></pre>
      </div>
      <div className={styles.windowFooter}>
        <span><i className={styles.statusDot} /> type checked</span>
        <span>{demo.language}</span>
      </div>
    </div>
  );
}

function HomepageHeader() {
  const mascot = useBaseUrl('/img/logo.svg');

  return (
    <header className={styles.hero}>
      <div className={styles.heroGrid} aria-hidden="true" />
      <div className={styles.orbOne} aria-hidden="true" />
      <div className={styles.orbTwo} aria-hidden="true" />
      <div className={`container ${styles.heroInner}`}>
        <div className={styles.heroCopy}>
          <div className={styles.eyebrow}><span>Open source</span> · Built for .NET + JavaScript</div>
          <Heading as="h1" className={styles.heroTitle}>
            Put a <em>real language</em><br />inside your product.
          </Heading>
          <p className={styles.heroLead}>
            FScript is a small, statically typed, embeddable language for the parts of your
            product that need to change faster than your application.
          </p>
          <div className={styles.heroActions}>
            <Link className={styles.primaryAction} to="/sandbox">
              Run it in your browser <Arrow />
            </Link>
            <Link className={styles.secondaryAction} to="/manual/learn/quickstart">
              Start the 5-minute tour
            </Link>
          </div>
          <div className={styles.installLine}>
            <span className={styles.prompt}>$</span>
            <code>dotnet tool install --global MagnusOpera.FScript</code>
          </div>
          <div className={styles.heroProof}>
            <span>Hindley–Milner inference</span>
            <span>Pattern matching</span>
            <span>Host-controlled I/O</span>
          </div>
        </div>
        <div className={styles.heroVisual}>
          <div className={styles.mascotMark}>
            <img src={mascot} alt="" width="92" height="92" />
            <span>YOUR APP</span>
          </div>
          <DemoWindow />
        </div>
      </div>
      <div className={styles.heroTicker} aria-label="FScript qualities">
        <div>
          <span>EMBED IT</span><i>✦</i><span>TYPE IT</span><i>✦</i><span>CONTROL IT</span><i>✦</i>
          <span>SHIP IT</span><i>✦</i><span>EMBED IT</span><i>✦</i><span>TYPE IT</span><i>✦</i>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  return (
    <Layout
      title="FScript — the embeddable typed language"
      description="Put a compact, statically typed scripting language inside your .NET or JavaScript product.">
      <HomepageHeader />
      <main className={styles.main}>
        <section className={styles.statementSection}>
          <div className={`container ${styles.statementGrid}`}>
            <p className={styles.sectionKicker}>Why FScript</p>
            <div>
              <Heading as="h2" className={styles.statementTitle}>
                Your users need flexibility.<br />Your system needs boundaries.
              </Heading>
              <p className={styles.statementCopy}>
                Most embedded scripting is a trade: power for safety, or simplicity for expressiveness.
                FScript gives you a compact ML-style language with static checks and an explicit capability boundary.
              </p>
            </div>
          </div>
          <div className={`container ${styles.metricStrip}`}>
            <div><strong>01</strong><span>Runs in your process<br />No sidecar. No RPC.</span></div>
            <div><strong>02</strong><span>Checks before execution<br />Types with no ceremony.</span></div>
            <div><strong>03</strong><span>You define the surface<br />No ambient authority.</span></div>
          </div>
        </section>

        <section className={styles.examplesSection}>
          <div className={`container ${styles.sectionHeading}`}>
            <p className={styles.sectionKicker}>Language, meet product</p>
            <Heading as="h2">Small syntax.<br /><em>Serious leverage.</em></Heading>
            <p>Readable enough for a rule. Expressive enough for a workflow.</p>
          </div>
          <div className={`container ${styles.exampleGrid}`}>
            {examples.map((example) => (
              <article className={styles.exampleCard} key={example.number}>
                <div className={styles.exampleTopline}>
                  <span>{example.number}</span><i />
                </div>
                <Heading as="h3">{example.title}</Heading>
                <p>{example.description}</p>
                <pre><code>{example.code}</code></pre>
              </article>
            ))}
          </div>
          <div className={`container ${styles.examplesLink}`}>
            <Link to="/manual/examples/guided-examples">Explore guided examples <Arrow /></Link>
          </div>
        </section>

        <section className={styles.boundarySection}>
          <div className={`container ${styles.boundaryGrid}`}>
            <div className={styles.boundaryCopy}>
              <p className={styles.sectionKicker}>The host stays in charge</p>
              <Heading as="h2">A language with<br />a <em>visible boundary.</em></Heading>
              <p>
                Core FScript has no secret door to your filesystem, network, or database. Your host exposes typed
                extern functions; scripts use exactly those capabilities and nothing more.
              </p>
              <ul>
                <li><span>01</span> Register typed host functions</li>
                <li><span>02</span> Parse and type-check the script</li>
                <li><span>03</span> Invoke explicit exports</li>
              </ul>
              <Link className={styles.textLink} to="/manual/embedding/overview">See the embedding model <Arrow /></Link>
            </div>
            <div className={styles.boundaryDiagram}>
              <div className={styles.hostBox}>
                <small>YOUR HOST</small>
                <strong>.NET / JS</strong>
                <div><span>Database</span><span>HTTP</span><span>Events</span></div>
              </div>
              <div className={styles.bridge}>
                <span>typed externs</span><i>→</i><span>explicit exports</span>
              </div>
              <div className={styles.languageBox}>
                <small>EMBEDDED</small>
                <strong>FScript</strong>
                <div><span>Rules</span><span>Flows</span><span>Logic</span></div>
              </div>
              <div className={styles.boundaryLabel}>CAPABILITY BOUNDARY</div>
            </div>
          </div>
        </section>

        <section className={styles.learningSection}>
          <div className={`container ${styles.learningHeader}`}>
            <div>
              <p className={styles.sectionKicker}>A gentler learning curve</p>
              <Heading as="h2">From zero to embedded<br />in three clean moves.</Heading>
            </div>
            <p>No archaeology required. Start by changing a working example, learn the language as you need it, then connect your host.</p>
          </div>
          <div className={`container ${styles.learningGrid}`}>
            <Link className={styles.learningCard} to="/sandbox">
              <span className={styles.learningNumber}>01</span>
              <div className={styles.learningIcon}>▶</div>
              <Heading as="h3">Play</Heading>
              <p>Run and remix real FScript in your browser. Nothing to install.</p>
              <b>Open sandbox <Arrow /></b>
            </Link>
            <Link className={styles.learningCard} to="/manual/learn/language-tour">
              <span className={styles.learningNumber}>02</span>
              <div className={styles.learningIcon}>ƒ</div>
              <Heading as="h3">Learn</Heading>
              <p>Tour the syntax through values, functions, data, and pattern matching.</p>
              <b>Take the tour <Arrow /></b>
            </Link>
            <Link className={styles.learningCard} to="/manual/embedding/real-world-embedding">
              <span className={styles.learningNumber}>03</span>
              <div className={styles.learningIcon}>{'{ }'}</div>
              <Heading as="h3">Embed</Heading>
              <p>Load a script, expose capabilities, and invoke typed exports from your app.</p>
              <b>Build the bridge <Arrow /></b>
            </Link>
          </div>
        </section>

        <section className={styles.ctaSection}>
          <div className={`container ${styles.ctaInner}`}>
            <img src={useBaseUrl('/img/logo.svg')} alt="FScript mascot" width="150" height="150" />
            <div>
              <p className={styles.sectionKicker}>Your product. Their logic.</p>
              <Heading as="h2">Stop hard-coding<br />the changeable parts.</Heading>
            </div>
            <div className={styles.ctaActions}>
              <Link className={styles.primaryAction} to="/manual/learn/quickstart">Get started <Arrow /></Link>
              <a className={styles.secondaryAction} href="https://github.com/MagnusOpera/FScript">View on GitHub</a>
            </div>
          </div>
        </section>
      </main>
    </Layout>
  );
}
