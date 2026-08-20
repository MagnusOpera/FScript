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
    title: 'Name every outcome',
    description: 'Use records and unions to describe the data a rule accepts and every result it can return.',
    code: `type Decision =\n  | Approve of int\n  | Review of string\n  | Decline\n\nlet decide score =\n  match score with\n  | n when n > 80 -> Approve 5000\n  | n when n > 55 -> Review "manual"\n  | _ -> Decline`,
  },
  {
    number: '02',
    title: 'Choose what scripts can call',
    description: 'Register typed host functions for the exact operations a script needs. Leave everything else out.',
    code: `let notifyLateInvoice invoice =\n  match invoice.Status with\n  | "late" ->\n      Host.notify invoice.Owner\n        $"Invoice {invoice.Id} is late"\n  | _ ->\n      ()`,
  },
  {
    number: '03',
    title: 'Call exports from your app',
    description: 'Load a script, find its exported functions, and invoke them by name from .NET or JavaScript.',
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
          <div className={styles.eyebrow}><span>Open source</span> · Embeds in .NET and JavaScript</div>
          <Heading as="h1" className={styles.heroTitle}>
            Change the rules.<br /><em>Keep the host.</em>
          </Heading>
          <p className={styles.heroLead}>
            Load <code>.fss</code> files, check their types, and run them inside your process.
            Your application decides which functions scripts can call.
          </p>
          <div className={styles.heroActions}>
            <Link className={styles.primaryAction} to="/sandbox">
              Try FScript <Arrow />
            </Link>
            <Link className={styles.secondaryAction} to="/manual/learn/quickstart">
              Install and run
            </Link>
          </div>
          <div className={styles.installLine}>
            <span className={styles.prompt}>$</span>
            <code>dotnet tool install --global MagnusOpera.FScript</code>
          </div>
          <div className={styles.heroProof}>
            <span>Static type inference</span>
            <span>Pattern matching</span>
            <span>Explicit host functions</span>
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
      <div className={styles.heroTicker} aria-label="How FScript runs">
        <div>
          <span>LOAD .FSS</span><i>✦</i><span>CHECK TYPES</span><i>✦</i><span>CALL EXPORTS</span><i>✦</i>
          <span>REGISTER EXTERNS</span><i>✦</i><span>LOAD .FSS</span><i>✦</i><span>CHECK TYPES</span><i>✦</i>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  return (
    <Layout
      title="FScript, a typed language for embedded rules"
      description="Load typed FScript rules inside a .NET or JavaScript application and control the functions they can call.">
      <HomepageHeader />
      <main className={styles.main}>
        <section className={styles.statementSection}>
          <div className={`container ${styles.statementGrid}`}>
            <p className={styles.sectionKicker}>Why embed a language?</p>
            <div>
              <Heading as="h2" className={styles.statementTitle}>
                A pricing change should not require a new application build.
              </Heading>
              <p className={styles.statementCopy}>
                Put changeable rules in <code>.fss</code> files. FScript checks them before execution,
                while your host keeps ownership of the database, network, filesystem, and other I/O.
              </p>
            </div>
          </div>
          <div className={`container ${styles.metricStrip}`}>
            <div><strong>01</strong><span>Runs in process<br />No service to operate.</span></div>
            <div><strong>02</strong><span>Checks types first<br />Errors stop before evaluation.</span></div>
            <div><strong>03</strong><span>Calls registered externs<br />The host controls I/O.</span></div>
          </div>
        </section>

        <section className={styles.examplesSection}>
          <div className={`container ${styles.sectionHeading}`}>
            <p className={styles.sectionKicker}>What the code looks like</p>
            <Heading as="h2">Typed data.<br /><em>Direct code.</em></Heading>
            <p>Functions, records, unions, and pattern matching. No class hierarchy required.</p>
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
              <p className={styles.sectionKicker}>The host sets the boundary</p>
              <Heading as="h2">Scripts get what<br /><em>you register.</em></Heading>
              <p>
                A script cannot query your database or send an HTTP request unless your host gives it a function
                that does so. Each extern has a name and a checked type signature.
              </p>
              <ul>
                <li><span>01</span> Register the externs the script needs</li>
                <li><span>02</span> Load and type-check the source</li>
                <li><span>03</span> Invoke a named export</li>
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
                <div><span>Data</span><span>Rules</span><span>Exports</span></div>
              </div>
              <div className={styles.boundaryLabel}>CAPABILITY BOUNDARY</div>
            </div>
          </div>
        </section>

        <section className={styles.learningSection}>
          <div className={`container ${styles.learningHeader}`}>
            <div>
              <p className={styles.sectionKicker}>Start with running code</p>
              <Heading as="h2">Try one rule. Then<br />wire it into your host.</Heading>
            </div>
            <p>The browser sandbox runs without an install. When the syntax makes sense, follow the host example for .NET or JavaScript.</p>
          </div>
          <div className={`container ${styles.learningGrid}`}>
            <Link className={styles.learningCard} to="/sandbox">
              <span className={styles.learningNumber}>01</span>
              <div className={styles.learningIcon}>▶</div>
              <Heading as="h3">Play</Heading>
              <p>Edit an FScript example and run it in the browser. Nothing to install.</p>
              <b>Open sandbox <Arrow /></b>
            </Link>
            <Link className={styles.learningCard} to="/manual/learn/language-tour">
              <span className={styles.learningNumber}>02</span>
              <div className={styles.learningIcon}>ƒ</div>
              <Heading as="h3">Learn</Heading>
              <p>Learn bindings, functions, data types, and pattern matching with working examples.</p>
              <b>Take the tour <Arrow /></b>
            </Link>
            <Link className={styles.learningCard} to="/manual/embedding/real-world-embedding">
              <span className={styles.learningNumber}>03</span>
              <div className={styles.learningIcon}>{'{ }'}</div>
              <Heading as="h3">Embed</Heading>
              <p>Load a script, register externs, and call an exported function from your app.</p>
              <b>Read the host example <Arrow /></b>
            </Link>
          </div>
        </section>

        <section className={styles.ctaSection}>
          <div className={`container ${styles.ctaInner}`}>
            <img src={useBaseUrl('/img/logo.svg')} alt="FScript mascot" width="150" height="150" />
            <div>
              <p className={styles.sectionKicker}>Run the first script</p>
              <Heading as="h2">See if FScript fits<br />your host.</Heading>
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
