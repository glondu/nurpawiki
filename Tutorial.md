# First page #

The first thing to do after installing and starting Nurpawiki is to point your browser at http://localhost:8080/view?p=WikiStart.

You will arrive at a login screen.  Type in "admin" as your login and leave the password empty.  That's the default after a fresh install.

You will be greeted with an empty page:

![http://nurpawiki.googlecode.com/svn/wiki/img/empty_nurpawiki.png](http://nurpawiki.googlecode.com/svn/wiki/img/empty_nurpawiki.png)

Click the "Create new page" link to create & edit the main page:

![http://nurpawiki.googlecode.com/svn/wiki/img/edited_start.png](http://nurpawiki.googlecode.com/svn/wiki/img/edited_start.png)

..and after pressing save:

![http://nurpawiki.googlecode.com/svn/wiki/img/saved_start.png](http://nurpawiki.googlecode.com/svn/wiki/img/saved_start.png)

# Adding to-dos #

We will now showcase the to-do feature of this wiki.  Let's make a page called "NurpaWiki" and place the following contents on it:

```
= NurpaWiki project =

This page is used to take notes & draft actions for the Nurpawiki SW project.

== Ocsigen related ==
* [todo Send an introduction e-mail to Ocsigen mailing list once build flow & documentation is good enough for public review]

== Web documentation ==
* [todo Write a short description of Nurpawiki on the Project Home]
* [todo Take screenshots of Nurpawiki todo list]
* [todo Take screenshots of Nurpawiki scheduler]
```

You will note that the wikitext conforms mostly to the one used in MediaWiki.  However, there is one notable exception: the `[todo <text>]` tag.

When you embed a `[todo]` in your wiki page, it will be picked up by the wiki parser and added to your global to-do list when you save the wiki page.

Here's a screenshot to illustrate what happens after you've saved your wiki page along with your to-do items:

![http://nurpawiki.googlecode.com/svn/wiki/img/todolist1.png](http://nurpawiki.googlecode.com/svn/wiki/img/todolist1.png)

Note the to-do list table that appeared on the left navbar.  The to-do items you inserted into your wikitext got added onto your to-do list!  Their appearance also changed on the wiki page to indicate that they're tasks embedded onto your page.

You can edit a task's priority by clicking the up/down arrows.  This will move it up or down on the left to-do list.  You can also mark it as completed by clicking the checkbox by the priority up/down buttons.  When you mark a to-do as completed, it will disappear from your to-do list but it will remain grayed on your wiki page.

## Moving tasks to different wiki pages ##
After saving the wiki page, you will note that your `[todo <text>]` tags got modified to include a task ID.  For example, a to-do in wikitext might now look like `[todo:4 Take screenshots of Nurpawiki todo list]`.  The number in `[todo:N <...>]` is the task ID and is used to uniquely identify a certain to-do item.

If you want to move a certain to-do item to another wikipage, just copy&paste the `[todo:N <...>]` text into another wiki page and save.

A single to-do can reside on several different wiki pages.  There's no reason to limit a to-do's existence to only a single page.  In fact, it's very useful to associate a given task with several contexts (wiki pages).

# Scheduling tasks #
To-do lists have a tendency to become very long over time.  The longer your to-do list gets, the harder it is to choose which tasks to complete any given day.  Prioritizing them helps, but even your high priority list might become long.  To reduce your daily load, you can trigger tasks to only appear on your to-do list after a certain date.

Another, perhaps more important use-case for to-do scheduling is reminder-style tasks that cannot really be completed until a certain date.  You don't want to see such tasks on your to-do list before you can actually do something about it but you still want to be reminded about it once you can actually do something about it.

Nurpawiki has a feature called the Scheduler that can be used to move tasks around in time in order to have them appear on your to-do list only after a certain date.

## Scheduler ##
Click on the "Scheduler" link on the upper left corner of the web page.  Here's where you're taken:

![http://nurpawiki.googlecode.com/svn/wiki/img/scheduler.png](http://nurpawiki.googlecode.com/svn/wiki/img/scheduler.png)

### Scheduling a single to-do ###
To schedule a to-do, click on the small pencil icon to the left of a todo to edit it.  You will be taken to a "to-do editor":

![http://nurpawiki.googlecode.com/svn/wiki/img/todo_edit_schedule.png](http://nurpawiki.googlecode.com/svn/wiki/img/todo_edit_schedule.png)

To edit the trigger date, click on the edit box below the to-do description.  A date selector popup will appear (see the above screenshot).  Select the date you want to assign to the task and click Save.

The above image show a task I cannot at the moment finish as I'm waiting for a certain GODI package to get fixed before I can proceed with my announcement.  I can just postpone the task for a few days.  It will pop up on my to-do list after a few days.  If the GODI package is then already working, I can complete the task and mark it as completed.  If I'm still unable to complete it, I can postpone it again using the Scheduler.

### Scheduling a batch of tasks ###
Sometimes it's useful to assign a larger group of tasks the same trigger date.  To do this, tick off the checkboxes for the tasks you want to move and click the "Mass edit" button.

# History #
Sometimes it's useful to look back at your week and recall what wiki pages you worked on, what notes you were taking or what tasks you created or completed.

Click on the History button to see a log of latest action in your wiki.