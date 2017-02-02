
namespace :data do
  desc <<-DESC
    Upload the data to the db
    DESC
  task :upload do
    #on roles(:app) do
      #within fetch(:app_path, release_path) do
      #end
      #sh "echo", "#{???_path}"
      sh "rsync", "-avh", "--progress", "sampling/slice_samples", "torsnet6cs@sliceplorer.cs.univie.ac.at:#{shared_path}"
    #end
  end

  desc <<-DESC
    link the shared data directory to the app server
    DESC
  task :link do
    on roles(:app) do
      within fetch(:app_path, release_path) do
        datapath = "#{shared_path}/slice_samples"
        execute :ln, "-s", datapath, "slice_samples"
      end
    end
  end

end

