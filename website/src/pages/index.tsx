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
        <section className={styles.languageScene}>
          <div className={`container ${styles.sceneIntro}`}>
            <p className={styles.sectionLabel}>Language lineage</p>
            <Heading as="h2">Name the thing.<br /><em>Or just ask for its shape.</em></Heading>
            <p className={styles.sceneLead}>
              FScript keeps the part of the ML family that makes programs pleasant to reason about: inferred
              types, immutable data, unions, curried functions, and pattern matching. Then it adds a useful choice.
              A record can carry a domain name, or a function can accept any value with the fields it needs.
            </p>
          </div>
          <div className={`container ${styles.typeCanvas}`}>
            <div className={styles.namedType}>
              <span className={styles.canvasKicker}>A name with meaning</span>
              <pre><code>{`type Customer =
  { Name: string
    Plan: string
    Seats: int }`}</code></pre>
              <span className={styles.inferredType}>Customer</span>
            </div>
            <div className={styles.typeFlow} aria-hidden="true">
              <span>Name</span><i />
              <span>Plan</span><i />
              <span>Seats</span><i />
            </div>
            <div className={styles.structuralType}>
              <span className={styles.canvasKicker}>A function with modest demands</span>
              <pre><code>{`let quote
  (buyer: {| Plan: string
             Seats: int |}) =
  price buyer.Plan * buyer.Seats`}</code></pre>
              <span className={styles.inferredType}>{`{| Plan: string; Seats: int |} → int`}</span>
            </div>
            <p className={styles.typeCaption}>
              <strong><code>quote customer</code> works.</strong> The function sees the shape it asked for. The rest of
              <code> Customer</code> stays where it belongs.
            </p>
          </div>
          <div className={`container ${styles.sceneLink}`}>
            <Link className={styles.inlineLink} to="/manual/learn/language-tour">Meet the language <Arrow /></Link>
          </div>
        </section>

        <section className={styles.embeddingScene}>
          <div className={`container ${styles.embeddingGrid}`}>
            <div className={styles.embeddingCopy}>
              <p className={styles.sectionLabel}>Interpreted embedding</p>
              <Heading as="h2">Teach a small language the verbs of your application.</Heading>
              <p>
                Your application already knows how to find a customer, price an order, or schedule a job. Expose
                those operations as typed functions and FScript turns them into a vocabulary for changeable rules.
              </p>
              <p>
                Scripts are loaded, checked, and called in-process. They compose the capabilities you provide and
                export ordinary functions back to the host. Your infrastructure remains yours. The policy becomes
                something people can read, test, and replace.
              </p>
              <Link className={styles.lightLink} to="/manual/embedding/overview">Build a language inside your application <Arrow /></Link>
            </div>
            <div className={styles.domainOrbit} aria-label="Host functions become a typed FScript vocabulary">
              <div className={styles.orbitRing} aria-hidden="true" />
              <div className={`${styles.orbitVerb} ${styles.verbOne}`}><code>Customer.find</code><span>host function</span></div>
              <div className={`${styles.orbitVerb} ${styles.verbTwo}`}><code>Quote.save</code><span>host function</span></div>
              <div className={`${styles.orbitVerb} ${styles.verbThree}`}><code>Audit.write</code><span>host function</span></div>
              <div className={styles.scriptCore}>
                <span>pricing.fss</span>
                <strong>your rules</strong>
                <code>Customer → Quote</code>
              </div>
              <div className={styles.exportBeam}><span>[&lt;export&gt;] quote</span><i aria-hidden="true">→</i><strong>YOUR APP</strong></div>
            </div>
          </div>
        </section>

        <section className={styles.javascriptScene}>
          <div className={`container ${styles.javascriptGrid}`}>
            <div className={styles.browserStage} aria-label="FScript running in a browser">
              <div className={styles.browserChrome}><i /><i /><i /><span>localhost / rules</span></div>
              <div className={styles.browserBody}>
                <div className={styles.browserEditor}>
                  <span>discount.fss</span>
                  <pre><code>{`let discount customer =
  match customer.Plan with
  | "scale" -> 20
  | "team"  -> 10
  | _       -> 0`}</code></pre>
                </div>
                <div className={styles.browserResult}>
                  <span>TYPE</span><code>Customer → int</code>
                  <span>RESULT</span><strong>20</strong>
                </div>
              </div>
              <div className={styles.browserStatus}><span><i /> checked in this tab</span><span>Fable · ESM</span></div>
            </div>
            <div className={styles.javascriptCopy}>
              <p className={styles.sectionLabel}>JavaScript support</p>
              <Heading as="h2">Put the language where the idea happens.</Heading>
              <p>
                Fable compiles the parser, type checker, and evaluator ahead of time to JavaScript modules. A browser,
                Node application, or playground can load FScript source and get the same types and language semantics
                without calling a server.
              </p>
              <p>
                The language engine ships as ordinary ESM, so production tooling can bundle and minify it alongside
                the rest of the application. This website's sandbox is the proof: the editor and the language run
                together in your tab.
              </p>
              <Link className={styles.inlineLink} to="/manual/embedding/fable-javascript">Take FScript to JavaScript <Arrow /></Link>
            </div>
          </div>
        </section>

        <section className={styles.goScene}>
          <div className={`container ${styles.goIntro}`}>
            <div>
              <p className={styles.sectionLabel}>Go support</p>
              <Heading as="h2">A script goes in.<br /><em>A native program walks out.</em></Heading>
            </div>
            <p>
              The Go backend does not drag an interpreter into the binary. It closes the inferred types across the
              whole program, removes unreachable work, simplifies calls, and turns proven tail recursion into loops.
              What remains is direct, typed Go.
            </p>
          </div>
          <div className={`container ${styles.goWorkbench}`}>
            <div className={styles.goSource}>
              <span className={styles.workbenchLabel}>FScript knows</span>
              <code>list&lt;int&gt;</code>
              <code>option&lt;Customer&gt;</code>
              <code>int → int</code>
            </div>
            <div className={styles.optimizerTrack} aria-label="Go compiler optimizations">
              <span>specialize types</span>
              <span>prune dead code</span>
              <span>reduce calls</span>
              <span>lower tail loops</span>
            </div>
            <div className={styles.goOutput}>
              <span className={styles.workbenchLabel}>Go receives</span>
              <code>[]int64</code>
              <code>option_Customer</code>
              <code>func(int64) int64</code>
            </div>
            <div className={styles.binaryStamp} aria-hidden="true"><span>GO</span><strong>01</strong><small>standalone</small></div>
          </div>
          <div className={`container ${styles.goFooter}`}>
            <p>Concrete slices. Concrete structs. Native functions. Separate source files, one optimized package.</p>
            <Link className={styles.darkLink} to="/manual/compiler/go">Compile your first Go project <Arrow /></Link>
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
