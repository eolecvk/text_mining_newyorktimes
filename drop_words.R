library(tm)

headline_drops_strings <- c(
  "movie listings",
  "a guide to",
  "gallery listings",
  "spare times for children",
  "the top 10 audiobooks",
  "re reading",
  "s on tv",
  "business highlights",
  "Today in history",
  "new york today",
  "your friday briefing",
  "the latest:",
  "today in politics:"
)

snippet_drops_strings <- c(
  "a guide to movies playing",
  "a guide to stand-up",
  "by amy pollock",
  "by matthew stock",
  "by jim drury",
  "get recommendations from new york times",
  "television highlights.",
  "your daily look at late-breaking news",
  "this word has appeared"
)


STOP_WORDS <- c(
  stopwords(kind="SMART"),
  # Common numbers, ordering words
  "one", "two", "three", "four", "ten", "fifty", "hundred",'hundr', "thousand", "million", "billion",
  "first", "second", "third", "fourth", "tenth", "fifti", "dozen",
  "next", "previous", "past", "times",
  # individual related
  'man', 'woman', 'mr', 'mrs', 'people', 'boy', 'girl','boys', 'girls', "miss", "mister",
  'individual','individu',
  # Time, calendar values
  'minute', 'hour',"day", "week", "weekend", "month", "year", 'annual',
  "today", "yesterday",'tomorrow', "night", "everyday",
  "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
  "mon", "tue", "wed", "thu", "fri", "sat", "sun",
  "jan","feb","mar","apr" ,"may", "jun", "jul","aug", "sep", "oct", "nov", "dec",
  "january", "february", "march", "april", "june", "july","august", "september",
  "october", "november", "december",
  
  'when',
  # Common adjectives, nouns, adverbs
  "new",'newer', "news", 'recent',"old", "start", "end", "back", 'good',"best", "longer", "better", "perfect",
  'bit', 'lot', 'biggest','rest', 'beneath', 'around', 'about', 'surround', 'basic', 'alongsid', 'closer', 'earli',
  "close", "slow", "fast", "quick", "late", "latest","time", "top", "list", "small","big", "large",
  "short", "long", "direct", "part", 'lack', 'newest', 'high', 'low',
  "thing", "stuff","place", "brief",'briefli',"set", "sets", "fair", "call",
  "los", "las", "san", "percent", "pre", "real", "really", "actually", "actual",
  'street', 'avenu', 'city','citi', 'address','mike', 'state', 'inside', 'insid', 'appar', 'ago',
  'key', 'countri','member', 'air', 'choic', 'articl', 'group',
  # Common verbs
  "will","went","can","said","say","take","get", "increas", "decreas", 
  "may", "might","must", "talk","win", "winner", "won", "help", "present", 'answer',
  "find","eat","open", "need", "take", "use", "work", "show", "shows",
  "look","make","give","move","run", "decide", "respond", 'shot', 'shoot',
  "add", "act", "feel",  "leads", "added", "adds", "rise", "speak",
  "write", "wrote", "wasn", "weren", "name", "don", "amp", "isn",
  "choose", "chose", "made", "stay", "staying", "left", "call",
  "happen", "spend", "faced", "met", "meet", "offer", "felt", "drop",
  "couldn", "mustn", "could", "must", "did", "didn", "require", "requires",  "required",
  "interest","interested","interesting", "think", "thought", "suggest", "mention", "build",
  "understand", "accept",'learn', 'explain', 'raise', 'raise', 
  'includ', 'includes', 'include', 'including', 'face', 'put', 'note',
  'begin', 'continu', 'creat', 'hope', 'teach', 'draw', 'base', 'play', 'built',
  'lead', 'stop', 'found','expect', 'stand', 'bring', 'brought', 'thought', 'reach', 'attempt',
  'surpris', 'announc', 'aim', 'rang', 'range', 'affect', 'impact', 'develop', 'descend', 'featur',
  'plan',
  # Film related noisy terms
  "reviews","review","box", "office","show",
  # Search terms
  "robot", "robots", "machine", "machines", "machina", "futur", "nyt",
  
  #MAYBES
  "technolog","technology", "tech", "human", "author",
  "world", "north", "south", "east", "west", '$'
)


save(file = '/home/eolus/Dropbox/MA710 Perso/ASSG2/Workspace/drop_word.RData',
     list = c('headline_drops_strings','snippet_drops_strings','STOP_WORDS'))