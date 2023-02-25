library(data.table)
library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(ggthemr)
library(ggrepel)
library(scales)


# customized settings for graph -------------------------------------------

ggthemr(
  palette = "flat",
  spacing = 1.15
)
myTheme1 <- theme(
  plot.title = element_text(size = 18),
  plot.title.position = "plot",
  plot.caption = element_text(hjust = 0, face = "italic", size = 8),
  plot.caption.position = "plot"
)


# importing data ----------------------------------------------------------

channels <- fread("channels.csv")
chat_stats <- fread("chat_stats.csv")
superchat_stats <- fread("superchat_stats.csv")
hololiveID_colors <- fread("hololiveID_colors.csv")


# What are VTubers? -------------------------------------------------------

channels_stats <- channels %>% 
  filter(group != "INACTIVE") %>% 
  group_by(affiliation) %>% 
  summarize(
    talentCount = uniqueN(channelId),
    totalSubscriber = sum(subscriptionCount),
    totalVideo = sum(videoCount)
  ) %>% 
  arrange(desc(totalSubscriber))

plot1 <- channels %>% 
  filter(group != "INACTIVE") %>% 
  mutate(Type = if_else(!str_detect(str_to_lower(affiliation), "independent"), "Corporate", "Indie")) %>% 
  filter(str_detect(str_to_lower(englishName), "hololive", negate = TRUE)) %>% 
  group_by(Type) %>% 
  slice_max(order_by = subscriptionCount, n = 10, with_ties = FALSE) %>% 
  ggplot(aes(
    x = fct_reorder(englishName, subscriptionCount), 
    y = subscriptionCount,
    fill = Type
  )) +
  geom_col(width = .75, show.legend = FALSE) +
  geom_text(
    aes(label = if_else(
      subscriptionCount >= 1e6,
      number(subscriptionCount, scale = 1e-6, accuracy = .1, suffix = "M"),
      number(subscriptionCount, scale = 1e-3, accuracy = 1, suffix = "K")
    ), color = Type),
    hjust = -.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~Type, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(c(0, .15), 0)
  ) +
  labs(
    title = "Most Subscribed VTubers",
    caption = "*) As per July 2022",
    y = "Total Subscribers",
  ) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold", size = 16)
  ) +
  myTheme1
print(plot1)

plot2 <- channels_stats %>% 
  mutate(Type = if_else(!str_detect(str_to_lower(affiliation), "independent"), "Corporate VTuber", "Indie Vtuber")) %>% 
  filter(Type == "Corporate VTuber") %>% 
  slice_max(order_by = totalSubscriber, n = 5) %>% 
  ggplot(aes(
    x = fct_reorder(affiliation, totalSubscriber, .desc = TRUE), 
    y = totalSubscriber
  )) +
  geom_col(width = .75) +
  geom_text(
    aes(label = number(totalSubscriber, scale = 1e-6, accuracy = .1, suffix = "M")),
    color = swatch()[2],
    vjust = -1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(c(0, .1), 0)
  ) +
  labs(
    title = "Subscribers Count by VTubers Agencies",
    caption = "*) As per July 2022",
    y = "Total Subscribers"
  ) +
  theme(
    axis.title.x = element_blank()
  ) +
  myTheme1
print(plot2)


# Introduction to Hololive ID ---------------------------------------------

hololiveID_channels <- channels[affiliation == "Hololive"][str_detect(group, "Indonesia")]

plot3 <- hololiveID_channels %>% 
  left_join(hololiveID_colors, by = "englishName") %>% 
  ggplot() +
  geom_col(aes(
    x = fct_reorder(englishName, subscriptionCount, .desc = TRUE), 
    y = subscriptionCount
  ), width = .75) +
  geom_label_repel(aes(
    x = fct_reorder(englishName, subscriptionCount, .desc = TRUE), 
    y = subscriptionCount,
    label = number(subscriptionCount, scale = 1e-3, accuracy = 1, suffix = "K", big.mark = "")
  ), color = swatch()[2]) +
  geom_line(
    aes(
      x = fct_reorder(englishName, subscriptionCount, .desc = TRUE), 
      y = videoCount*2e3,
      group = 1
    ),
    color = swatch()[1],
    linewidth = 1,
    lty = "dashed"
  ) +
  geom_point(
    aes(
      x = fct_reorder(englishName, subscriptionCount, .desc = TRUE), 
      y = videoCount*2e3,
    ), 
    color = swatch()[1],
    size = 4
  ) +
  geom_label_repel(aes(
    x = fct_reorder(englishName, subscriptionCount, .desc = TRUE), 
    y = videoCount*2e3,
    label = videoCount
  ), color = swatch()[1]) +
  scale_y_continuous(
    labels = label_number(scale = 1e-3, suffix = "K", big.mark = ""),
    expand = expansion(c(0, .1), 0),
    sec.axis = sec_axis(~./2e3, name = "Video Count")
  ) +
  labs(
    title = "hololive ID Talents Channel Stats",
    y = "Subscribers Count",
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(face = "bold", colour = swatch()[2]),
    axis.title.y.right = element_text(face = "bold", color = swatch()[1]),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  myTheme1
print(plot3)


# Live Streaming and Chats ------------------------------------------------

hololiveID_chat_stats <- chat_stats %>% 
  semi_join(hololiveID_channels, by = "channelId") %>% 
  mutate(
    chatPerPerson = chats/uniqueChatters,
    chatPerMember = memberChats/uniqueMembers
  ) %>% 
  group_by(channelId) %>% 
  summarize(across(chats:chatPerMember, mean, na.rm = TRUE)) %>% 
  left_join(
    hololiveID_channels %>% 
      select(channelId, englishName),
    by = "channelId"
  ) %>% 
  left_join(hololiveID_colors, by = "englishName")

plot4 <- hololiveID_chat_stats %>% 
  ggplot(aes(x = chats, y = uniqueChatters, color = primaryColor)) +
  geom_point(size = 3, alpha = .75) +
  geom_text_repel(aes(label = englishName, fontface = "bold"), hjust = 0) +
  scale_color_identity() +
  scale_x_continuous(labels = number_format(scale = 1e-3, suffix = "K", big.mark = "")) +
  scale_y_continuous(labels = number_format(scale = 1e-3, suffix = "K", big.mark = "")) +
  labs(
    title = "hololive ID Live Chats Stats",
    x = "Monthly Average Live Chats Count",
    y = "Monthly Average Live Chatters Count"
  ) +
  myTheme1
print(plot4)

hololiveID_chat_stats_growth <- chat_stats %>% 
  semi_join(hololiveID_channels, by = "channelId") %>% 
  group_by(channelId) %>% 
  arrange(channelId, period) %>% 
  mutate(across(c(chats, uniqueChatters, bannedChatters), function(x) {x/lag(x, 1)-1})) %>% 
  ungroup() %>% 
  group_by(channelId) %>% 
  summarize(across(c(chats, uniqueChatters), mean, na.rm = TRUE)) %>% 
  rename_with(.fn = ~str_c(., "_growth"), .cols = -channelId) %>% 
  left_join(
    hololiveID_channels %>% 
      select(channelId, englishName),
    by = "channelId"
  ) %>% 
  left_join(hololiveID_colors, by = "englishName")

plot5 <- hololiveID_chat_stats_growth %>% 
  select(englishName, chats_growth, uniqueChatters_growth) %>% 
  pivot_longer(-englishName) %>% 
  ggplot(aes(x = englishName, y = value, color = name, group = name)) +
  geom_point(size = 3) +
  geom_line(aes(group = name, lty = name), linewidth = 1) +
  geom_label_repel(aes(group = name, label = percent(value, accuracy = .01)), show.legend = FALSE) +
  scale_y_continuous(
    labels = label_percent(),
    breaks = (1/4)*c(0:4)
  ) +
  scale_color_discrete(labels = c("Chats", "Chatters")) +
  scale_linetype_discrete(labels = c("Chats", "Chatters")) +
  labs(
    title = "hololive ID Live Chats Growth Stats",
    x = "Live Chats Count",
    y = "Live Chatters Count",
    color = "Legend",
    linetype = "Legend"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(face = "bold"),
    axis.title.y.right = element_text(face = "bold", color = "grey"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  myTheme1
print(plot5)

# Membership --------------------------------------------------------------

plot6 <- hololiveID_chat_stats %>% 
  ggplot(aes(x = memberChats, y = uniqueMembers, color = primaryColor)) +
  geom_point(size = 3, alpha = .75) +
  geom_text_repel(aes(label = englishName, fontface = "bold"), hjust = 0) +
  scale_color_identity() +
  scale_x_continuous(labels = number_format(scale = 1e-3, suffix = "K", big.mark = "")) +
  labs(
    title = "hololive ID Live Chats Stats (Member Only)",
    x = "Monthly Average Live Chats Count",
    y = "Monthly Average Live Chatters Count"
  ) +
  myTheme1
print(plot6)

plot7 <- hololiveID_chat_stats %>% 
  select(englishName, chatPerPerson, chatPerMember) %>% 
  pivot_longer(-englishName) %>% 
  ggplot(aes(x = fct_reorder(englishName, value, .desc = TRUE), y = value, fill = name)) +
  geom_col(position = "stack", width = .75) +
  scale_y_continuous(
    expand = expansion(c(0, .1), 0)
  ) +
  scale_fill_discrete(
    labels = c("Monthly Average Chat per Person (Member only)", "Monthly Average Chat per Person (All)")
  ) +
  geom_hline(
    aes(yintercept = mean(hololiveID_chat_stats$chatPerMember)), 
    color = "black", 
    lty = "dashed",
    linewidth = 1
  ) +
  geom_hline(
    aes(yintercept = mean(hololiveID_chat_stats$chatPerPerson)), 
    color = "white", 
    lty = "dashed",
    linewidth = 1
  ) +
  annotate(
    geom = "label", x = 9.5, y = mean(hololiveID_chat_stats$chatPerMember),
    label = "Average Chat per Person (Member only)",
    fontface = "bold", size = 4, hjust = 1
  ) +
  annotate(
    geom = "label", x = 9.5, y = mean(hololiveID_chat_stats$chatPerPerson),
    label = "Average Chat per Person (All)",
    fontface = "bold", size = 4, hjust = 1
  ) +
  labs(
    title = "hololive ID Average Live Chats per Person",
    subtitle = "Overall vs Member",
    y = "Value",
    fill = "Legend"
  ) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  myTheme1 
print(plot7)


# Super Chats -------------------------------------------------------------

hololiveID_superchat_stats <- superchat_stats %>% 
  semi_join(hololiveID_channels, by = "channelId") %>% 
  mutate(superChatAmountPerPerson = totalSC/uniqueSuperChatters) %>% 
  group_by(channelId) %>% 
  summarize(across(c(superChats, uniqueSuperChatters, totalSC, superChatAmountPerPerson), mean, na.rm = TRUE)) %>% 
  left_join(
    hololiveID_channels %>% 
      select(channelId, englishName),
    by = "channelId"
  ) %>% 
  left_join(hololiveID_colors, by = "englishName")

plot8 <- hololiveID_superchat_stats %>% 
  ggplot(aes(x = superChats, y = uniqueSuperChatters, color = primaryColor)) +
  geom_point(aes(size = superChatAmountPerPerson), alpha = .75) +
  geom_text_repel(aes(label = englishName, fontface = "bold"), hjust = 0) +
  scale_color_identity() +
  labs(
    title = "hololive ID Super Chats Stats",
    x = "Monthly Average Super Chats Count",
    y = "Monthly Average Super Chatters Count",
    size = "Average Super Chats Amount per Person (in JPY)"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  myTheme1
print(plot8)

plot9 <- hololiveID_superchat_stats %>% 
  ggplot(aes(x = fct_reorder(englishName, totalSC, .desc = TRUE), y = totalSC)) +
  geom_col(width = 0.75) +
  geom_text(aes(
    label = number(totalSC, scale = 1e-3, prefix = "¥", suffix = "K", big.mark = "")),
    vjust = -1,
    fontface = "bold",
    color = swatch()[2]
  ) +
  scale_y_continuous(
    labels = number_format(scale = 1e-3, suffix = "K", big.mark = ""),
    expand = expansion(c(0, .1), 0)
  ) +
  geom_hline(
    aes(yintercept = mean(totalSC)), 
    color = "black", 
    lty = "dashed",
    linewidth = 1
  ) +
  annotate(
    geom = "label", x = 9.5, y = mean(hololiveID_superchat_stats$totalSC),
    label = str_c("Average Amount: ¥", 
                  number(mean(hololiveID_superchat_stats$totalSC), scale = 1e-3, accuracy = .1, suffix = "K")),
    fontface = "bold", size = 4, hjust = 1
  ) +
  labs(
    title = "hololive ID Monthly Super Chats Amount",
    y = "Value (in JPY)",
  ) +
  theme(
    axis.title.x = element_blank()
  ) +
  myTheme1
print(plot9)


# Audience Sentiment ------------------------------------------------------

hololiveID_sentiment <- chat_stats %>% 
  semi_join(hololiveID_channels, by = "channelId") %>% 
  mutate(
    deletedChatsRate = deletedChats/(chats+deletedChats)*1e5,
    bannedChattersRate = bannedChatters/(uniqueChatters+bannedChatters)*1e4
  ) %>% 
  group_by(channelId) %>% 
  summarize(across(contains("Rate"), mean, na.rm = TRUE)) %>% 
  left_join(
    hololiveID_channels %>% 
      select(channelId, englishName),
    by = "channelId"
  ) %>% 
  left_join(hololiveID_colors, by = "englishName") %>% 
  select(englishName, primaryColor, deletedChatsRate, bannedChattersRate)

plot10 <- hololiveID_sentiment %>% 
  pivot_longer(-c(englishName, primaryColor)) %>% 
  mutate(value = if_else(str_detect(name, "deleted"), value, -value)) %>% 
  ggplot(aes(x = fct_reorder(englishName, value, .desc = TRUE), y = value, fill = name)) +
  geom_col(width = .75, position = position_stack(vjust = 1)) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(-120, 120, 40),
    labels = abs(seq(-120, 120, 40)),
    limits = c(-120, 120)
  ) +
  scale_fill_manual(
    values = c(swatch()[9], swatch()[4]),
    labels = c("Banned", "Deleted")
  ) +
  scale_color_manual(
    values = c(swatch()[9], swatch()[4]),
    labels = c("Banned", "Deleted")
  ) +
  labs(
    title = "hololive ID Audience Sentiment",
    y = "Rate",
    fill = "Legend"
  ) +
  annotate(
    geom = "text", x = 8.1, y = -95,
    label = "Highest Banned\nChatters Rate\n\n\nBanned Chatters\nper 10K Chatters",
    fontface = "bold", hjust = 0.5, color = "black"
  ) +
  annotate(
    geom = "segment", x = 9, xend = 8.2, y = -50, yend = -75,
    arrow = arrow(length = unit(.015,"npc"), type = "open"),
    size = 1.2
  ) +
  annotate(
    geom = "text", x = 8.1, y = -95, 
    label = number(max(hololiveID_sentiment$bannedChattersRate), accuracy = 1),
    fontface = "bold", color = swatch()[9], size = unit(9, "npc"),
  ) +
  annotate(
    geom = "text", x = 3.6, y = 100,
    label = "Highest Deleted\nChats Rate\n\n\nDeleted Chats\nper 100K Chats",
    fontface = "bold", hjust = 0.5, color = "black"
  ) +
  annotate(
    geom = "segment", x = 1.6, xend = 2.3, y = 90, yend = 100,
    arrow = arrow(length = unit(.015,"npc"), type = "open"),
    size = 1.2
  ) +
  annotate(
    geom = "text", x = 3.6, y = 100, 
    label = number(max(hololiveID_sentiment$deletedChatsRate), accuracy = 1),
    fontface = "bold", color = swatch()[4], size = unit(9, "npc"),
  ) +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  myTheme1
print(plot10)


# PCA Biplot Analysis -----------------------------------------------------

hololiveID_chat_stats_processed <- chat_stats %>% 
  semi_join(hololiveID_channels, by = "channelId") %>% 
  mutate(
    memberChats_rate = memberChats/chats,
    uniqueMembers_rate = uniqueMembers/uniqueChatters,
    chat_per_person = chats/uniqueChatters,
    memberchat_per_person = memberChats/uniqueMembers,
    bannedChatters_rate = bannedChatters/(uniqueChatters+bannedChatters),
    deletedChats_rate = deletedChats/(chats+deletedChats)
  )

hololiveID_chat_stats_aggregated <- hololiveID_chat_stats_processed %>% 
  group_by(channelId) %>% 
  summarize(across(-period, mean, na.rm = TRUE))

hololiveID_superchat_stats_processed <- superchat_stats %>% 
  semi_join(hololiveID_channels, by = "channelId") %>% 
  left_join(
    chat_stats %>% 
      select(channelId, period, uniqueChatters),
    by = c("channelId", "period")
  ) %>% 
  mutate(
    SCamountPerPerson = totalSC/uniqueSuperChatters, 
    SCunique_rate = uniqueSuperChatters/uniqueChatters
  )

hololiveID_superchat_stats_aggregated <- hololiveID_superchat_stats_processed %>% 
  group_by(channelId) %>% 
  summarize(across(c(SCamountPerPerson, SCunique_rate), mean, na.rm = TRUE))

## Feature selection
holoID_colors <- hololiveID_colors$primaryColor
names(holoID_colors) <- hololiveID_colors$englishName

selected_feature <- hololiveID_channels %>% 
  left_join(hololiveID_chat_stats_aggregated, by = "channelId") %>% 
  left_join(hololiveID_superchat_stats_aggregated, by = "channelId") %>% 
  left_join(
    hololiveID_chat_stats_growth %>% 
      select(-englishName), 
    by = "channelId"
  ) %>% 
  select(
    name = englishName,
    `Chatters engagement` = chats, 
    `Chatters engagement growth` = chats_growth,
    `Chatters reach` = uniqueChatters, 
    `Chatters reach growth` = uniqueChatters_growth,
    `Members engagement` = memberChats_rate, 
    `Members reach` = uniqueMembers_rate,
    `Interaction from chatters` = chat_per_person, 
    `Interaction from members` = memberchat_per_person,
    `Chatters willingness to pay` = SCamountPerPerson,
    `Lots of spenders` = SCunique_rate
  )

## Correlation Heatmap
selected_feature %>% 
  select(-name) %>% 
  cor() %>% 
  corrplot(method = "color", order = "AOE")

## Assigning PCA
PCAresult <- selected_feature %>% 
  column_to_rownames("name") %>% 
  PCA(graph = FALSE)

## PCA result eigen values
PCAresult$eig

## Screeplot
fviz_screeplot(PCAresult, addlabels = TRUE)

## Biplot
fviz_pca_biplot(PCAresult, repel = TRUE) +
  geom_point(
    aes(color = rownames(PCAresult$call$X)), 
    size = 3.5, show.legend = FALSE
  ) +
  scale_color_manual(
    labels = rownames(PCAresult$call$X),
    values = holoID_colors
  )
