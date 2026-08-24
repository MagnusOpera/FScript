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
          <div className={styles.eyebrow}><span>Open source</span> · Embed in .NET and JavaScript · Compile to Go</div>
          <Heading as="h1" className={styles.heroTitle}>
            Change the rules.<br /><em>Keep the host.</em>
          </Heading>
          <p className={styles.heroLead}>
            Load <code>.fss</code> files, check their types, and run them inside your process. Or compile
            them into a standalone Go project.
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
            <span>Versioned IR</span>
            <span>Go backend</span>
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
          <span>LOAD .FSS</span><i>✦</i><span>CHECK TYPES</span><i>✦</i><span>BUILD HIR</span><i>✦</i>
          <span>EXECUTE LIR</span><i>✦</i><span>GENERATE GO</span><i>✦</i><span>LOAD .FSS</span><i>✦</i>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  return (
    <Layout
      title="FScript: embed typed rules or compile them to Go"
      description="Run typed FScript rules in .NET or JavaScript, execute validated LIR, or generate standalone Go source.">
      <HomepageHeader />
      <main className={styles.main}>
        <section className={styles.introduction}>
          <div className="container">
            <p className={styles.sectionLabel}>Language lineage</p>
            <div className={styles.introductionGrid}>
              <Heading as="h2">ML roots. Less luggage.</Heading>
              <div className={styles.prose}>
                <p>
                  Its syntax and type system draw from F# and OCaml. Hindley-Milner inference gives functions
                  static types without requiring annotations on every binding.
                </p>
                <p>
                  Records, discriminated unions, immutable collections, curried functions, and pattern matching
                  do most of the work. FScript leaves out the object system and the larger standard library.
                </p>
              </div>
            </div>
            <div className={styles.codeStudy}>
              <pre><code>{`type Payment =
  { Customer: string
    Amount: int
    Attempts: int }

type Action =
  | Capture
  | RetryAfter of int
  | AskForReview

let decide payment =
  match payment with
  | { Amount = amount } when amount > 10000 ->
      AskForReview
  | { Attempts = attempts } when attempts < 3 ->
      RetryAfter 60
  | _ ->
      Capture`}</code></pre>
              <aside className={styles.languageNotes}>
                <Heading as="h3">An ML-style function</Heading>
                <p><code>Payment</code> fixes the available fields and their types. A misspelled field fails during loading.</p>
                <p><code>Action</code> fixes the return cases. <code>RetryAfter</code> must carry an integer.</p>
                <p>The compiler infers <code>decide : Payment -&gt; Action</code> from the function body.</p>
                <Link className={styles.inlineLink} to="/manual/learn/language-tour">Read the language tour <Arrow /></Link>
              </aside>
            </div>
          </div>
        </section>

        <section className={styles.hostSection}>
          <div className={`container ${styles.hostGrid}`}>
            <div className={styles.hostCopy}>
              <p className={styles.sectionLabel}>Interpreted embedding</p>
              <Heading as="h2">Change the script. Leave the process alone.</Heading>
              <p>
                The .NET runtime parses and checks an <code>.fss</code> file before evaluating it. The host calls
                exported functions and registers the external functions that scripts may use. There is no worker
                process or network boundary.
              </p>
              <p>
                FScript cannot discover your database or HTTP client. If the host does not register an operation,
                that operation does not exist in the script environment.
              </p>
              <Link className={styles.inlineLink} to="/manual/embedding/overview">How embedding works <Arrow /></Link>
            </div>
            <div className={styles.contract} aria-label="Example host contract">
              <div className={styles.contractHeading}><span>Registered functions</span><span>pricing.fss</span></div>
              <code>Customer.find : string -&gt; Customer option</code>
              <code>Quote.save : Quote -&gt; bool</code>
              <code>Audit.write : string -&gt; unit</code>
              <p>The script sees these names and types. The host implements them.</p>
            </div>
          </div>
        </section>

        <section className={styles.useSection}>
          <div className="container">
            <div className={styles.useRows}>
              <article>
                <div>
                  <p className={styles.sectionLabel}>JavaScript support</p>
                  <Heading as="h2">The whole interpreter fits in the browser.</Heading>
                </div>
                <div>
                  <p>Fable compiles the FScript parser, type checker, and evaluator to JavaScript. The website sandbox uses that build in the browser. It interprets FScript source rather than generating JavaScript with <code>eval</code>.</p>
                  <Link className={styles.inlineLink} to="/manual/embedding/fable-javascript">Use FScript from JavaScript <Arrow /></Link>
                </div>
              </article>
              <article>
                <div>
                  <p className={styles.sectionLabel}>Go support</p>
                  <Heading as="h2">Types in. Native binary out.</Heading>
                </div>
                <div>
                  <p>The Go backend specializes inferred types across the whole program. It emits concrete structs, slices, functions, and tagged unions. Imported FScript files become separate Go files in one package.</p>
                  <Link className={styles.inlineLink} to="/manual/compiler/go">Compile to Go <Arrow /></Link>
                </div>
              </article>
            </div>
          </div>
        </section>

        <section className={styles.finalLinks}>
          <div className={`container ${styles.finalLinksInner}`}>
            <p>FScript is open source.</p>
            <nav aria-label="Start using FScript">
              <Link to="/sandbox">Open the sandbox</Link>
              <Link to="/manual/learn/quickstart">Install and run</Link>
              <a href="https://github.com/MagnusOpera/FScript">GitHub <Arrow /></a>
            </nav>
          </div>
        </section>
      </main>
    </Layout>
  );
}
