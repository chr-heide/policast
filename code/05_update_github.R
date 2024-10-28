
library(git2r)

#repo_path <- "."  # The path to repo - only define if different form cwd

repo <- repository()
add(repo, web_dir)
commit(repo, "Update plots")

# Push the changes to GitHub
push(repo, credentials = cred_token("GITHUB_PAT"))
