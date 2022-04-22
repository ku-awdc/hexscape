## Probability of splitting:

### Splitting for females:

patch_capacity <- 10
patch_breeding_females <- 5:30

f_split_int <- qlogis(0.05)
f_split_coef <- log(1.5)

# The animal-level probability of wanting to split:
split_prob <- ifelse(patch_capacity >= patch_breeding_females,
	0,
	plogis(f_split_int + f_split_coef * (patch_breeding_females - patch_capacity -1L))  # NB: add 1 so that coef = 0 for 1 more pig
)

plot(patch_breeding_females, split_prob, type='s')


### Splitting for males:

patch_breeding_females <- 10
patch_breeding_males <- 1:10

m_split_int <- qlogis(0.05)
m_split_coef <- log(1.30)
mating_ratio <- 5

# The animal-level probability of wanting to split:
split_prob <- ifelse(patch_breeding_females >= (mating_ratio * patch_breeding_males),
	0,
	plogis(m_split_int + m_split_coef * (patch_breeding_males - (patch_breeding_females/mating_ratio) - 1L))  # NB: add 1 so that coef = 0 for 1 more pig
)

plot(patch_breeding_males, split_prob, type='s')


## Then we need to make a decision if splitting actually happens, e.g.:
patch_capacity <- 10
patch_breeding_females <- 20
split_prob <- ifelse(patch_capacity >= patch_breeding_females,
	0,
	plogis(f_split_int + f_split_coef * (patch_breeding_females - patch_capacity -1L))  # NB: add 1 so that coef = 0 for 1 more pig
)
group_size <- rbinom(1, patch_breeding_females-patch_capacity, split_prob)
# If the group_size is less than 2 then splitting does not happen, otherwise it has some relationship that we already worked out
group_size <- 0:10
group_prob <- pmin(1, pmax(0, log(group_size)/2))
plot(group_size, group_prob, type='s')


## Probability of settling

# Settling can NEVER happen if the carrying capacity is zero.  Otherwise it can happen for two reasons:

# 1.  Based on attractiveness / spare capacity:

### Settling for females

patch_capacity <- 10
patch_breeding_females <- 5:30
group_breeding_females <- 5  # Intentionally ignored, but must be >0
group_breeding_males <- 2  # Intentionally ignored, can be 0 or >0

f_settle_int <- qlogis(0.5)
f_settle_coef <- log(2)

# The group-level probability of wanting to settle:
settle_prob_attractive <- ifelse(patch_capacity <= patch_breeding_females,
	0,
	plogis(f_settle_int + f_settle_coef * (patch_capacity - patch_breeding_females -1L))  # NB: add 1 so that coef = 0 for 1 more pig
)

plot(patch_breeding_females, settle_prob_attractive, type='s')


### Settling for males

patch_breeding_females <- 20
mating_ratio <- 5
patch_breeding_males <- 1:10
group_breeding_females <- 0  # Must be 0
group_breeding_males <- 2  # Intentionally ignored, must be >0

m_settle_int <- qlogis(0.5)
m_settle_coef <- log(2)

# The group-level probability of wanting to settle:
settle_prob_attractive <- ifelse(patch_breeding_females <= (mating_ratio * patch_breeding_males),
	0,
	plogis(m_settle_int + m_settle_coef * ((patch_breeding_females/mating_ratio) - patch_breeding_males - 1L))  # NB: add 1 so that coef = 0 for 1 more pig
)

plot(patch_breeding_males, settle_prob_attractive, type='s')



# 2.  Based on distance travelled:

## Note: this hazard stuff is broken, but the principle is correct ... and we can just replace it with a vector of probabilities anyway

movements <- 1:1000
settle_intercept <- -3.942065
settle_rho <- 1.5

# Problem: this hazard is not bounded at 0,1 so not correct?
settle_prob_distance <- settle_rho * movements^(settle_rho-1) * exp(settle_intercept)
plot(movements, settle_prob_distance, type='s')

movements^settle_rho * exp(settle_intercept)

cumset <- 0
for(i in 1:length(movements)){
	cumset <- cumset + (settle_prob_distance * (1-cumset))
}
plot(movements, cumset, type="l")

cum_settle_hazard <- 1 - exp( exp(settle_intercept) * -((movements +1)^settle_rho - movements ^settle_rho))
plot(movements, cum_settle_hazard, type='s')


## Overall settling prob:
settle_prob <- 1 - ((1 - settle_prob_attractive) * (1 - settle_prob_distance))
