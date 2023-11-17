From: Brian Fannin <bfannin@casact.org>
Date: On Monday, January 30th, 2023 at 9:29 AM
Subject: RE: Technology survey meeting
To: adam@adamleerich.com <adam@adamleerich.com>

No problem. Kevin and I talked a bit about some basic analysis I did on usage patterns. None of the results are statistically significant, but:
•	Tableau is down
•	SQL is moving to the middle – some more people using, some people using less frequently
•	SAS is down
•	Excel is down, but barely. Weirdly, because it has such a high percentage of daily users, even a small taper gets close to statistical significance.
 
Brian Fannin
tel 1.919.457.3439
 



------- Original Message -------
On Friday, November 4th, 2022 at 12:51 PM, Brian Fannin <bfannin@casact.org> wrote:


All,
 
Earlier this week, I had an opportunity to tidy up the data input and wrangling code. Things are now set up so that we can create data which has a column for the survey year. I’ve pushed those change out to GitHub.
 
To stoke your interest in the results – and to give a rare example of me using Excel for data analysis – I’ve attached a pivot table which compares usage by tool for 2021 and 2022. SAS declines a bit, Python goes up a bit, as do PowerBI and GoogleSheets. The attached workbook was based on CSV files that are generated from the data wrangling code, which is all out on GitHub.
 
Please let me know if you have thoughts or questions.
 
Regards,
Brian Fannin
tel 1.919.457.3439
 


From: Brian Fannin 
Sent: Friday, October 28, 2022 11:31 AM
To: adam@adamleerich.com; Kevin McBeth <Kevin.McBeth@coherent.global>; Louise <louise_francis@msn.com>
Cc: Lederer, Julie <julie.lederer@insurance.mo.gov>
Subject: Actuarial technology survey
 
All,
 
Sorry for the radio silence over the past couple months. The survey concluded last week and I now have the data file containing the responses. Early next week, I’ll add this to the GitHub repo where the data is analyzed. That repo may be found here: https://github.com/casact/technology_survey. It’s public, so you can look through any and all of the code which was used to create the survey report: https://www.casact.org/abstract/first-annual-cas-actuarial-technology-survey. You may notice some analyses which didn’t find their way into the final report. There’s cope to revisit these and/or consider other sorts of analysis.
 
The project uses the four-stage work flow from the R package `represtools`. It shouldn’t be too weird to try and follow if you’ve never used this approach. The package web page has a short overview: https://pirategrunt.com/represtools/. One practical hurdle may be that `represtools` assumes that you have the “make” program installed and available on your device. If you’re on Linux (and probably Mac OS?) you’re already sorted. If you’re on Windows, then you should be fine if you’ve got Rtools installed. If “make” is present, then within RStudio you can open the project, hit CTRL+SHIFT+B and all of the code will run in the exact order needed to create the final report. I’m assuming it won’t be that smooth as there will probably be packages missing, or there’s some legacy bug that I’ll need to take care of.
 
Next steps:
1.	Clone the repo and start to get familiar with the workflow. 
2.	Ensure that you’ve got “make” installed and can build the output. If you hit an error, just give me a heads up.
3.	Wait for Brian to upload this year’s data along with cleaned up code to cook the raw data.
 
Regards,
Brian Fannin ACAS CSPA MAAA, Research Actuary
Casualty Actuarial Society • 4350 North Fairfax Drive, Suite 250 • Arlington, VA 22203
tel 1.919.457.3439 • bfannin@casact.org
 




