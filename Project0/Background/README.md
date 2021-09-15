# Background 

Cortisol and DHEA are two hormones that regulate our stress system. 
Cortisol is known to have a peak shortly after waking and then falls over the 
day to a low in the evening. Features of this diurnal pattern have been 
implicated in depression, sleep disorders and many other conditions. Being able 
to collect samples to measure this curve in a natural environment (e.g. at home)
is useful for research on this endocrine system. The accuracy of saliva sample c
ollection is crucial to obtaining reliable characterization of the cortisol 
awakening response and perhaps defining the diurnal decline in salivary cortisol. 

Investigator paragraph: We tested the use of a convenient and novel collection 
device for collecting saliva on strips of filter paper in a specially 
constructed booklet for determination of both salivary cortisol and DHEA.  In 
the present study, 31 healthy control subjects collected saliva samples four 
times a day for three days using the filter paper device (Saliva Procurement and
Integrated Testing (SPIT) booklet) which was maintained during the collection 
period in a large plastic bottle with an electronic monitoring cap. Subjects 
were asked to collect saliva samples at waking, 30 min later, before lunch and 
600 min after waking.  The time of waking and the time before lunch were allowed
to vary by the subjects’ schedules.
 
# Questions of Interest 
1. What is the agreement between the subject’s recordings of sampling times compared to the times recorded by an electronic monitoring cap? As the investigator, I imagine in my head a picture with the times from memscaps on one access and booklet times on the other access.  What I want to know is are
these times significantly correlated?  How much difference is there between the two times? How biased are the booklet times compared to the mems times?
2. Are subjects adhering accurately to the +30min and +10 hour sampling times required by a study protocol? How do we define adherent for Question 2? As the investigator, I would like to know the percentage of observations where the booklet or mems time is <=7.5 minutes from the 30 minutes protocol and the same for the 600 minutes.  A bit more lax definition would be to also record this information defining adherence as <=15minutes.
3. What are the changes of cortisol and DHEA over time? What is the time pattern expected for Cort and DHEA over time?  There is a sharp rise in Cort and probably DHEA between waking and 30 min post waking.  Then the levels decline to 10 hours past waking and plateau until very early morning.  I am interested in understanding whether the spit test shows a significant increase between waking and 30 minutes and the rate of decline after 30 minutes.  This would tell me that the spit test is able to capture a rise even under a less than perfect collection strategy (at home).
 
# Data Variable definitions
__Subjectid__: Subject number
__CollectionDate__: Date of sample collection formatted as mm/dd/yyyy
__Collection Sample__: ordering of samples over the day ranging from 1-4. 1 is 
at waking, 2 ~30 min from waking, 3 at ~lunch and 4 at ~10 hours after waking.
__Booklet: Clock time__: Time of day of sample collection noted by participant 
formatted as hh:mm in military time.
__MEMs: Clock time__: Time of day of sample collection by electronic cap stamp 
formatted as hh:mm in military time.
__Sleep Diary reported wake time__: Time of day of waking formatted as hh:mm in 
military time.
__Booklet: Sample interval__: Number of hours and minutes from waking recorded 
by participant formatted as hh:mm where waking was recorded as midnight 00:00
__Booklet: Sample interval Decimal Time (mins)__: Number of minutes from waking 
for the sample as recorded by participant.
__MEMs__: Sample interval: Number of hours and minutes from waking electronic 
cap stamp formatted as hh:mm where waking was recorded as midnight 00:00
__MEMs: Sample interval Decimal Time (mins)__: Number of minutes from waking 
electronic stamp.
__Cortisol (ug/dl)__: Cortisol levels in nanograms/deciliter units.
__DHEA (pg/dl)__: DHEA levels in picograms/deciliter units.
__Cortisol (nmol/L)__: Cortisol levels in a different unit.
__DHEA (nmol/L)__: DHEA levels in a different unit.
__DAYNUMBER__: Number of the day samples were collected. 1-3 days.
