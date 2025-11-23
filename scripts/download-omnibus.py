import requests
from time import sleep
from tqdm import tqdm
import json

def get_courses_for_code(roster, subject):
    req = requests.get(f'https://classes.cornell.edu/api/2.0/search/classes.json?roster={roster}&subject={subject}')
    return req.json()['data']['classes']

def get_subjects(roster):
    req = requests.get(f"https://classes.cornell.edu/api/2.0/config/subjects.json?roster={roster}")
    classes = req.json()["data"]["subjects"]
    return [x["value"] for x in classes]

data = []
roster = 'FA25'
subjects = get_subjects(roster)
for subject in tqdm(subjects):
    sleep(1)
    data.extend(get_courses_for_code(roster, subject))

with open('omnibus.json', 'w') as f:
    json.dump(data, f)
