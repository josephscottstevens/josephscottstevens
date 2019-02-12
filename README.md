# The Developer's Handbook

There are a lot of *pitfalls* and *counter-intuitive things* you'll run into as a developer, most of the time most we developers implicitly learn to navigate these traps by experience.

That being said, the Hope here is to provide a starting point for intuition. There a lot of **~~facts~~** in this document that aren't facts. The purpose is to specifically advice you on how to mentally understand and strategize when you run into these classic traps.

Traps & Pitfalls
* **Uncertainty:**  
Developers live in a world of uncertainty, and uncertainty is stressful and likely to make you feel bad. If your team isn't open and communicating about that uncertainty. That is un-healthy, you should be able to say, in the open. I just ran into this *higher order function*, what the heck is a higher order function? The team led must set an example that is perfectly okay thing to say in the open.

* **Insecurity:**  
New programmers are in a vulnerable position, and they depend on their team lead to succeed. These programmers should trust and learn from their lead, and bet on him succeeding. It is up to the newer programmers to do their best to help their lead succeed, to put in effort in learning as much as they can, they should be open minded and willing to work extra hard when the chance arises. If you feel that way, there a ton of other newer developers who feel the exact same way. And that is perfectly fine, and normal. It is temporary, and you'll be a kickass developer before you know it, I promise.

* **Adding Team Value**  
As a new programmer, the more you ask for help, the less useful to the team you are. However, if you never ask for help, you will fail, every time, all the time. It takes at least a year of solid effort to be able to master the skill of knowing when someone else can assist and when you spend the time to do it without help. Team leads should be checking AT LEAST once a day with each of their dev's, making sure they aren't very far off the track.

* **Adding Project Value**  
As developers, we are responsible for delivering features to the business. Businesses do not understand technology, and occasionally they make decisions that have un-favorable trade-offs to both parties.  
IE: *They may ask to ship a feature that will make the code base very difficult to work with go on, be quite clunky, cost a lot, and give low value*.  
That being said, that conversation has a very high chance of going poorly. You should pass the message up, and hope they listen, but at the end of the day, it isn't your call.

* **Language \ Framework wars:**  
Talking about what language is good, and what language is bad is likely to spark an argument that is based on internal values, and is unlikely to produce any positive results. *Here are some specific callouts.*  
1. As a developer, we should also not recommend languages with particularly bad reputations, such as PHP, COBOL, C++, C or Visual Basic. For largely political reasons, and also some decent technical reasons as well.  
2. We also should never recommend Angular to any clients. For nothing other than the reason that the version changes are frequent, non back-wards compatible and unless your company is willing to pay for a large re-write, you will get stuck on a later version. And once you aren't on the latest version, you are a working on a legacy tech stack, which sucks.

* **What language is best?**
The programming language you use is import and impacts your career, and is worth thinking about. That being said, the way the industry decides which language to choose is a popularity contest, and your vote doesn't count for much. *Very very roughly speaking:*  
20 years ago that was C++  
10 years ago that was Java\C#  
Currently it's JavaScript  
Developers rarely have the chance to influence these choices, that being said, we do occasionally get to choose what projects to be in. It is the best interest for the company, and the best interest of the developer, to pick something newer on the list. Older codebases in general are harder to work with, harder to ship features, and require large teams, and the risk of those teams failing is high.

* **Project Complexity:**
The complexity of a project is some combination of how well maintained the codebase is, how skilled the programmers were, and how much effort was put into keeping the code base simple. That being said, those are minor players when it comes to complexity.  
Project complexity down to how much debugging work has to be done to get a feature shipped and tested. The difficulty of that debugging is based on how many shared referenced to mutable things there are. If you pass an object to a function, and that function mutates something. You have added 1 point to complexity, the more connected and shared those references are, the faster the exponential nature of complexity arises.

* **Purpose Statement:** 
As a dev, generally devs are part of a team, within an organization. The success of the organization is with its devs and vice/versa. Success is built on stable growth. Stability is built on retention of good talent. Good talent stays when they are happy. People are generally happy when they are growing towards their goal and are recognized. Its important to ask questions others organizations are incapable of asking for a developer contracter is capable of moving its resources so that the resources can grow and have opportunities for recognition. 

There needs to be a bi-annual statement that is a statement FROM the developer. The first open question is "What is your purpose here?". Its a statement to get the expectation the developer has of oneself and career during their tenure at the organization. The second question is "What is stopping you from your purpose here?". This is to determine what the organization can feasibly do for the developer for retentention purposes. There is potential to ask yes/no questions following these statements, "Do you have what you need to meet your purpose?", "Are you on the right track for your purpose?", and "Are you being recognized for your work?". The questions should make the developer feel comfortable that the organization is intent on knowing what drives the developer and if there's anything the organization can do to make the develoepr content. The yes/no questions should exact the feeling at the moment after asking the question whether the organization should take immediate action. 
